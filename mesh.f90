MODULE mesh
USE constants
 IMPLICIT NONE

  INTRINSIC NINT

  INTEGER, PARAMETER :: meshver = 4
  INTEGER, PARAMETER :: mesh_bnd_none = 1,&
       mesh_bnd_xplane = 2,&
       mesh_bnd_yplane = 3,&
       mesh_bnd_zplane = 4,&
       mesh_bnd_prdx1 = 5,&
       mesh_bnd_prdx2 = 6,&
       mesh_bnd_prdy1 = 7,&
       mesh_bnd_prdy2 = 8,&
       mesh_bnd_rz1 = 9,&
       mesh_bnd_rz2 = 10
  INTEGER :: i,j,w
  TYPE node
     REAL (KIND=dp), DIMENSION(3) :: p
     INTEGER, DIMENSION(:), POINTER :: face_indices, node_indices
     INTEGER :: parent_index
     INTEGER :: bnd, nbnd
  END TYPE node

  TYPE face
     REAL (KIND=dp), DIMENSION(3) :: n, cp
     REAL (KIND=dp), DIMENSION(3,3) :: s, m
     INTEGER, DIMENSION(3) :: node_indices
     INTEGER, DIMENSION(3) :: edge_indices
     REAL (KIND=dp) :: area, pd
     INTEGER :: id
     INTEGER :: parent_index
  END TYPE face

  TYPE line
     INTEGER, DIMENSION(2) :: node_indices
     INTEGER :: id
  END TYPE line
! Edge de ejemplo
!  TYPE edge
!     INTEGER, DIMENSION(2) :: node_indices, bnode_indices, face_indices
!     REAL (KIND=dp) :: length
!!     REAL (KIND=dp), DIMENSION(2) :: rwgDiv
!     INTEGER :: bnd
!     INTEGER :: parent_index
!     INTEGER :: couple_index
!     INTEGER, DIMENSION(:,:), ALLOCATABLE :: child_indices ! (:,1)=submesh, (:,2)=local edge
!  END TYPE edge
! Edge mio
TYPE edge
     INTEGER, DIMENSION(2) :: node_indices, bnode_indices, face_indices
     REAL (KIND=dp) :: length
!     REAL (KIND=dp), DIMENSION(2) :: rwgDiv
     INTEGER :: bnd
     INTEGER :: parent_index
     INTEGER :: couple_index
     INTEGER, DIMENSION(:,:), ALLOCATABLE :: child_indices ! (:,1)=submesh, (:,2)=local edge
  END TYPE edge
! TYPE edge
!     INTEGER, DIMENSION(2) :: node_indices, face_indices
!     INTEGER :: edge_index ! -1 if not a boundary
 !    REAL (KIND=dp) :: longitud
 ! END TYPE edge
  ! Interior face.

  TYPE solid_face
     INTEGER, DIMENSION(3) :: node_indices
     INTEGER, DIMENSION(2) :: solid_indices, bnode_indices
     INTEGER :: face_index ! -1 if not a boundary
     REAL (KIND=dp) :: area
  END TYPE solid_face

  TYPE solid
     INTEGER, DIMENSION(4) :: node_indices
     INTEGER, DIMENSION(4) :: solid_face_indices
     REAL (KIND=dp) :: volume
     INTEGER :: id
  END TYPE solid

  TYPE mesh_container
     TYPE(node), DIMENSION(:), ALLOCATABLE :: nodes
     TYPE(face), DIMENSION(:), ALLOCATABLE :: faces
     TYPE(line), DIMENSION(:), ALLOCATABLE :: lines
     TYPE(edge), DIMENSION(:), ALLOCATABLE :: edges
     TYPE(solid), DIMENSION(:), ALLOCATABLE :: solids
     TYPE(solid_face), DIMENSION(:), ALLOCATABLE :: solid_faces
     INTEGER :: nnodes, nfaces, nlines, nedges, nsolids, nsolid_faces
     REAL (KIND=dp) :: avelen
  END TYPE mesh_container

CONTAINS
 ! Returns the three-letter filename extension of the filename.
  FUNCTION getext(filename) RESULT(ext)
    CHARACTER (LEN=*), INTENT(IN) :: filename
    CHARACTER (LEN=3) :: ext
    INTEGER :: n

    n = LEN_TRIM(filename)

    ext = filename((n-2):n)
  END FUNCTION getext

FUNCTION get_mesh_element_lines(filename) RESULT(lines)
    CHARACTER (LEN=*), INTENT(IN) :: filename
    INTEGER, DIMENSION(:,:), POINTER :: lines
    INTEGER :: nelements, element_number, iovar, fid = 10, n
    CHARACTER (LEN=256) :: lineid

    OPEN(fid, FILE=TRIM(filename), ACTION='READ', IOSTAT=iovar)
    IF(iovar>0) THEN
       WRITE(*,*) 'Could not open mesh file ', TRIM(filename), '!'
       STOP
    END IF

    iovar = 0
    DO WHILE(iovar==0)
       READ(fid, *, IOSTAT=iovar) lineid
       IF(lineid=='$Elements') THEN
          READ(fid, *, IOSTAT=iovar) nelements
          ALLOCATE(lines(nelements,1:2))
          DO n=1,nelements
             READ(fid, *, IOSTAT=iovar) element_number, lines(n,1), lines(n,2)
          END DO
          READ(fid, *, IOSTAT=iovar) lineid
          IF(lineid/='$EndElements') THEN
             WRITE(*,*) 'Could not find $EndElements specifier!'
             STOP
          END IF
       END IF
    END DO

    CLOSE(fid)
  END FUNCTION get_mesh_element_lines

SUBROUTINE load_mesh(filename,mesh)
    TYPE(mesh_container), INTENT(INOUT) :: mesh
    CHARACTER (LEN=*), INTENT(IN) :: filename
    !TYPE(mesh_container) :: mesh
    CHARACTER (LEN=3) :: ext

    ext = getext(filename)

    IF(ext=='msh') THEN
       mesh = load_mesh_gmsh(filename)
    ELSE
       WRITE(*,*) 'Unrecognized mesh file extension!'
       STOP
    END IF

    mesh%nsolid_faces = 0

    WRITE(*,*) 'Mesh file loaded successfully.'
    WRITE(*,'(A,I0,:,A)') ' - Read ', mesh%nnodes, ' nodes'
    WRITE(*,'(A,I0,:,A)') ' - Read ', mesh%nfaces, ' faces'
    WRITE(*,'(A,I0,:,A)') ' - Read ', mesh%nlines, ' lines'
    WRITE(*,'(A,I0,:,A)') ' - Read ', mesh%nsolids, ' solids'
    
  END SUBROUTINE load_mesh
 FUNCTION load_mesh_gmsh(filename) RESULT(mesh)
    CHARACTER (LEN=*), INTENT(IN) :: filename
    CHARACTER (LEN=256) :: lineid, line
    TYPE(mesh_container) :: mesh
    INTEGER :: fid = 10, iovar, nnodes, node_number, n,&
         nelements, element_number, element_type, cface, cline, csolid, ntags
    INTEGER, DIMENSION(10) :: element_data
    INTEGER, DIMENSION(:,:), POINTER :: element_types
    REAL (KIND=dp), DIMENSION(3) :: np
    CHARACTER (LEN=3) :: mshver

    element_types => get_mesh_element_lines(filename)

    OPEN(fid, FILE=TRIM(filename), ACTION='READ', IOSTAT=iovar)
    IF(iovar>0) THEN
       WRITE(*,*) 'Could not open mesh file! ',iovar
       STOP
    END IF

    iovar = 0
    DO WHILE(iovar==0)
       READ(fid, *, IOSTAT=iovar) lineid

       IF(lineid=='$MeshFormat') THEN
          READ(fid, '(A3)', IOSTAT=iovar) mshver
          IF(mshver/='2.1' .AND. mshver/='2.2') THEN
             WRITE(*,*) 'Mesh version is unsupported!'
             CLOSE(fid)
             STOP
          END IF

          WRITE(*,'(A,A3)') 'Mesh version: ', mshver

          READ(fid, *, IOSTAT=iovar) lineid
          IF(lineid/='$EndMeshFormat') THEN
             WRITE(*,*) 'Could not find $MeshFormatNodes specifier!'
             STOP
          END IF
       ELSE IF(lineid=='$Nodes') THEN
          READ(fid, *, IOSTAT=iovar) nnodes
          ALLOCATE(mesh%nodes(nnodes))

          DO n=1,nnodes
             READ(fid, *, IOSTAT=iovar) node_number, np(1:3)
             mesh%nodes(node_number)%p = np
          END DO

          READ(fid, *, IOSTAT=iovar) lineid
          IF(lineid/='$EndNodes') THEN
             WRITE(*,*) 'Could not find $EndNodes specifier!'
             STOP
          END IF
          mesh%nnodes = nnodes
       ELSE IF(lineid=='$Elements') THEN
          mesh%nfaces = COUNT(element_types(:,1)==2)
          mesh%nlines = COUNT(element_types(:,1)==1)
          mesh%nsolids = COUNT(element_types(:,1)==4)

          ALLOCATE(mesh%faces(1:mesh%nfaces))

          IF(mesh%nlines/=0) THEN
             ALLOCATE(mesh%lines(1:mesh%nlines))
          END IF

          IF(mesh%nsolids/=0) THEN
             ALLOCATE(mesh%solids(1:mesh%nsolids))
          END IF

          READ(fid, *, IOSTAT=iovar) nelements

          cface = 0
          cline = 0
          csolid = 0

          DO n=1,nelements
             ntags = element_types(n,2)

             IF(element_types(n,1)==2) THEN
                cface = cface + 1
                READ(fid, *, IOSTAT=iovar) element_data(1:(ntags+6))
                mesh%faces(cface)%id = element_data(4)
                mesh%faces(cface)%node_indices(1:3) = element_data((4+ntags):(6+ntags))
             ELSE IF(element_types(n,1)==1) THEN
                cline = cline + 1
                READ(fid, *, IOSTAT=iovar) element_data(1:(ntags+5))
                mesh%lines(cline)%id = element_data(4)
                mesh%lines(cline)%node_indices(1:2) = element_data((4+ntags):(5+ntags))
             ELSE IF(element_types(n,1)==4) THEN
                csolid = csolid + 1
                READ(fid, *, IOSTAT=iovar) element_data(1:(ntags+7))
                mesh%solids(csolid)%id = element_data(4)
                mesh%solids(csolid)%node_indices(1:4) = element_data((ntags+4):(ntags+7))
             ELSE
                READ(fid, *, IOSTAT=iovar) element_data(1)
             END IF
          END DO

          READ(fid, *, IOSTAT=iovar) lineid
          IF(lineid/='$EndElements') THEN
             WRITE(*,*) 'Could not find $EndElements specifier!'
             STOP
          END IF
       END IF
    END DO

    DEALLOCATE(element_types)

    CLOSE(fid)

    ! Change vertex index rotation.
  !  DO n=1,mesh%nfaces
  !     CALL swap(mesh%faces(n)%node_indices(2), mesh%faces(n)%node_indices(3))
  !  END DO


    !OPEN(10, FILE='nodes', ACTION='write')
    !DO n=1,mesh%nnodes
    !   WRITE(10,'(I5,3E15.3)') n, mesh%nodes(n)%p
    !END DO
    !CLOSE(10)

    !OPEN(10, FILE='faces', ACTION='write')
    !DO n=1,mesh%nfaces
    !   WRITE(10,'(I5,3I5)') n, mesh%faces(n)%node_indices
    !END DO
    !CLOSE(10)

  END FUNCTION load_mesh_gmsh

 SUBROUTINE export_mesh(filename, mesh)
    CHARACTER (LEN=*), INTENT(IN) :: filename
    TYPE(mesh_container), INTENT(IN) :: mesh
    INTEGER :: fid = 10, iovar, i

    OPEN(fid, FILE=TRIM(filename), ACTION='WRITE', IOSTAT=iovar)
    IF(iovar>0) THEN
       WRITE(*,*) 'Could not open mesh file for export!'
       STOP
    END IF

    WRITE(fid,'(A,T9,I0)') 'version', meshver

    WRITE(fid,'(A)') '[nodes]'
    WRITE(fid,*) mesh%nnodes
    DO i=1,mesh%nnodes
       WRITE(fid,*) mesh%nodes(i)%p
       WRITE(fid,*) mesh%nodes(i)%parent_index
    END DO

    WRITE(fid,'(A)') '[faces]'
    WRITE(fid,*) mesh%nfaces
    DO i=1,mesh%nfaces
       WRITE(fid,*) mesh%faces(i)%n
       WRITE(fid,*) mesh%faces(i)%cp
       WRITE(fid,*) mesh%faces(i)%s
       WRITE(fid,*) mesh%faces(i)%m
       WRITE(fid,*) mesh%faces(i)%node_indices
       WRITE(fid,*) mesh%faces(i)%edge_indices
       WRITE(fid,*) mesh%faces(i)%area
       WRITE(fid,*) mesh%faces(i)%pd
       WRITE(fid,*) mesh%faces(i)%id
       WRITE(fid,*) mesh%faces(i)%parent_index
    END DO

    WRITE(fid,'(A)') '[lines]'
    WRITE(fid,*) mesh%nlines
    DO i=1,mesh%nlines
       WRITE(fid,*) mesh%lines(i)%node_indices
       WRITE(fid,*) mesh%lines(i)%id
    END DO

    WRITE(fid,'(A)') '[edges]'
    WRITE(fid,*) mesh%nedges
    DO i=1,mesh%nedges
       WRITE(fid,*) mesh%edges(i)%node_indices
       WRITE(fid,*) mesh%edges(i)%bnode_indices
       WRITE(fid,*) mesh%edges(i)%face_indices
       WRITE(fid,*) mesh%edges(i)%length
       WRITE(fid,*) mesh%edges(i)%bnd
       WRITE(fid,*) mesh%edges(i)%parent_index
       WRITE(fid,*) mesh%edges(i)%couple_index
       WRITE(fid,*) SIZE(mesh%edges(i)%child_indices,1)
       IF(SIZE(mesh%edges(i)%child_indices,1)/=0) THEN
          WRITE(fid,*) mesh%edges(i)%child_indices(:,:)
       END IF
    END DO

    WRITE(fid,'(A)') '[solids]'
    WRITE(fid,*) mesh%nsolids
    DO i=1,mesh%nsolids
       WRITE(fid,*) mesh%solids(i)%node_indices
       WRITE(fid,*) mesh%solids(i)%solid_face_indices
       WRITE(fid,*) mesh%solids(i)%volume
       WRITE(fid,*) mesh%solids(i)%id
    END DO

    WRITE(fid,'(A)') '[solid faces]'
    WRITE(fid,*) mesh%nsolid_faces
    DO i=1,mesh%nsolid_faces
       WRITE(fid,*) mesh%solid_faces(i)%node_indices
       WRITE(fid,*) mesh%solid_faces(i)%solid_indices
       WRITE(fid,*) mesh%solid_faces(i)%bnode_indices
       WRITE(fid,*) mesh%solid_faces(i)%face_index
       WRITE(fid,*) mesh%solid_faces(i)%area
    END DO

    WRITE(fid,'(A)') '[end]'

    CLOSE(fid)
  END SUBROUTINE export_mesh
  SUBROUTINE scale_mesh(mesh, scale)
    TYPE(mesh_container), INTENT(INOUT) :: mesh
    REAL (KIND=dp), INTENT(IN) :: scale
    INTEGER :: n

    DO n=1,mesh%nnodes
       mesh%nodes(n)%p =  mesh%nodes(n)%p*scale
    END DO
  END SUBROUTINE scale_mesh

! funciones nuevas
!! ve si un valor esta en un arreglo
FUNCTION saberSista(array,valor) RESULT(si)
  INTEGER, DIMENSION(3),INTENT(in) :: array
  integer,INTENT(in) :: valor
  logical :: si
  do i=0,SIZE(array)
    if (array(i) == valor) then
       si = .True.
       STOP
    end if
  end do
  si = .false.
END FUNCTION
    
!! Verifica que un elemento este en un arreglo
!! ar el arreglo
!! val el valor
FUNCTION find(ar,val) result(si)
  INTEGER, INTENT(in), DIMENSION(2) :: ar
  integer, INTENT(in) :: val
  logical :: si
  do i=0,SIZE(ar)
    if (ar(i) == val) then
      si = .True.
      STOP
    end if
  end do

  si = .False.
end function
!! Saber si un elemento de un arreglo ya esta en otro
FUNCTION yaSeUso(nodos,nodo) result(si)
  type(mesh_container), INTENT(in) :: nodos
  integer, INTENT(in) :: nodo
  logical :: si
  do i=0,SIZE(nodos%edges)
    if (ALL(nodos%edges%node_indices(i)==nodo)) then
      si= .True.
      STOP
    end if
  si= .False.
  END DO
END FUNCTION
!> Obtienes los lados comunes de los triangulos
!! @param mesh el mesh_container
SUBROUTINE GET_EDGES(mesh)
    type(mesh_container),INTENT(INOUT) :: mesh
    !integer, intent(in) :: cantnodos
    !type(edge), dimension(:,:), allocatable :: result
    integer :: no
    do i=1,mesh%nnodes
        mesh%EDGES(i)%bnd = i
        do j=1,mesh%nfaces
           if(saberSista(mesh%faces(j)%node_indices, mesh%edges(i)%bnd)) then
             no=0
             do w=0,2
               if (mesh%EDGES(i)%bnd /= mesh%faces(j)%node_indices(w)) then
                 if(yaSeUso(mesh,mesh%faces(j)%node_indices(w))) then
                   if(find(mesh%EDGES(i)%node_indices, mesh%faces(j)%node_indices(w))) then
                     ! Ojala sirva esto
                     mesh%EDGES(i)%node_indices(no) =mesh%faces(j)%node_indices(w)
                     no = no+1
                   end if
                 end if
               end if
             END DO
           end if
        END DO
    
    END DO

END SUBROUTINE


END MODULE mesh