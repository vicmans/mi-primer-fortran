module funciones

CONTAINS 
FUNCTION saberSista(array,valor) RESULT(si)
  INTEGER, DIMENSION(3),INTENT(in) :: array
  real,INTENT(in) :: valor
  logical :: si
  do i=0,SIZE(array)
    if (array(i) == valor) then
       si = .True.
       STOP
    end if
  end do
  si = .false.
END FUNCTION
    

FUNCTION find(ar,val) result(si)
    INTEGER, INTENT(in), DIMENSION(2) :: ar
	real, INTENT(in) :: val
	logical :: si
	do i=0,SIZE(ar)
		if (ar(i) == val) then
			si = .True.
			STOP
		end if
	end do

	si = .False.
end function

FUNCTION yaSeUso(nodos,nodo) result(si)
	type(edge), INTENT(in) :: nodos
	real, INTENT(in) :: nodo
	logical :: si
	do i=0,SIZE(nodos)
		if(nodos%node_indices(i)==nodo) then
			si= .True.
			STOP
		end if
	si= .False.
	END DO
END FUNCTION

FUNCTION GET_EDGES(mesh,cantnodos) RESULT(result)
    type(mesh_container),INTENT(IN) :: mesh
    integer, intent(in) :: cantnodos
    type(edge), dimension(:,:), allocatable :: result
    integer :: no
    do i=0,nodes 
        result(i)%bnd = i+1
        do j=1,SIZE(mesh)
           if(saberSista(mesh%faces(j)%node_indices,result(i)%bnd)
             no=0
             do w=0,2
               if (result(i)%bnd /= mesh%faces(j)%node_indices(w))
                 if(yaSeUso(result(i),mesh%faces(j)%node_indices(w)))
                   if(find(result(i)%node_indices, mesh%faces(j)%node_indices(w)))
                     ! Ojala sirva esto
                     result(i)%node_indices(no) =mesh%faces(j)%node_indices(w)
                     no = no+1
                   end if
                 end if
               end if
             END DO
           end if
        END DO
    
    END DO

END FUNCTION
END module