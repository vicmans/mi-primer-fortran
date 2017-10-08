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

  TYPE node
     REAL (KIND=dp), DIMENSION(3) :: p
     !INTEGER, DIMENSION(:), POINTER :: face_indices, node_indices
     INTEGER :: parent_index
     !INTEGER :: bnd, nbnd
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

  TYPE edge
     INTEGER, DIMENSION(2) :: node_indices, bnode_indices, face_indices
     REAL (KIND=dp) :: length
!     REAL (KIND=dp), DIMENSION(2) :: rwgDiv
     INTEGER :: bnd
     INTEGER :: parent_index
     INTEGER :: couple_index
     INTEGER, DIMENSION(:,:), ALLOCATABLE :: child_indices ! (:,1)=submesh, (:,2)=local edge
  END TYPE edge

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

  ! Creates unique edges from the given nodes and faces. These edges
  ! are directly associated with the expansion coefficients of surface
  ! current densities.
  SUBROUTINE build_mesh(mesh, scale)
    TYPE(mesh_container), INTENT(INOUT) :: mesh
    REAL (KIND=dp), INTENT(IN) :: scale
    INTEGER :: n, m, l, cedge, nedges
    TYPE(edge), DIMENSION(:), ALLOCATABLE :: tmpedges
    LOGICAL :: found_edge
    INTEGER, DIMENSION(2) :: pair

    WRITE(*,*) 'Building mesh data'

    CALL scale_mesh(mesh, scale)

    ! Allocate edge reservoir with upper bound size.
    nedges = SIZE(mesh%faces)*3
    ALLOCATE(tmpedges(1:nedges))

    ! Declare node indices with undefined values.
    DO n=1,nedges
       tmpedges(n)%node_indices(:) = -1
       tmpedges(n)%bnode_indices(:) = -1
       tmpedges(n)%face_indices(:) = -1
    END DO

    cedge = 0

    ! Create unique edges.
    DO n=1,SIZE(mesh%faces)
       DO m=1,3

          ! This determined the edge node indexing.
          pair = (/mesh%faces(n)%node_indices(m), mesh%faces(n)%node_indices(indexrot3(m+1))/)

          ! Check if this edge already exists in the list.
          found_edge = .FALSE.
          DO l=1,cedge
             IF(cmp_pairs(tmpedges(l)%node_indices, pair)) THEN
                found_edge = .TRUE.

                ! Add this face index to edge's face list and the second one.
                ! If an edge is shared by more than two faces, only two connections
                ! are recorded.
                tmpedges(l)%face_indices(2) = n
                tmpedges(l)%bnode_indices(2) = mesh%faces(n)%node_indices(indexrot3(m+2))

                ! Add this edge index to face's edge list.
                mesh%faces(n)%edge_indices(m) = l

                EXIT
             END IF
          END DO

          IF(found_edge.eqv..FALSE.) THEN
             ! Add new edge.
             cedge = cedge + 1
             tmpedges(cedge)%node_indices = pair

             ! Add this face index to edge's face list as the first one.
             tmpedges(cedge)%face_indices(1) = n
             tmpedges(cedge)%bnode_indices(1) = mesh%faces(n)%node_indices(indexrot3(m+2))

             ! Add this edge index to face's edge list.
             mesh%faces(n)%edge_indices(m) = cedge
          END IF
       END DO
    END DO

    ! Trim edge arrays.
    mesh%nedges = cedge
    ALLOCATE(mesh%edges(1:cedge))
    DO n=1,cedge
       mesh%edges(n)%node_indices(:) = tmpedges(n)%node_indices(:)
       mesh%edges(n)%bnode_indices(:) = tmpedges(n)%bnode_indices(:)
       mesh%edges(n)%face_indices(:) = tmpedges(n)%face_indices(:)
    END DO

    ! Deallocate temporary arrays.
    DEALLOCATE(tmpedges)

    mesh%edges(:)%bnd = mesh_bnd_none

    ! Compute average edge length.
    mesh%avelen = average_edge_length(mesh)

    WRITE(*,'(A,I0,:,A)') ' - Created ', cedge, ' unique edges'

    !IF(mesh%nsolids>0) THEN
    !   CALL build_solid_faces(mesh)
    !END IF

    !CALL compute_basis_data(mesh)

    WRITE(*,*) 'Mesh data built successfully'

  END SUBROUTINE build_mesh

FUNCTION load_mesh(filename) RESULT(mesh)
    CHARACTER (LEN=*), INTENT(IN) :: filename
    TYPE(mesh_container) :: mesh
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
    
  END FUNCTION load_mesh
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

END MODULE mesh