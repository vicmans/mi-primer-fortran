module compute
  implicit none
  real :: 
  contains
    subroutine llenar_matriz(mesh,freq)
      implicit none
      real :: freq
      TYPE(mesh_container) :: mesh
        
      do m=1,mesh%nedges
        ! Obteniendo
        Ammas = mesh%faces(mesh%edges(m)%face_indices(1))%area
        Ammenos = mesh%faces(mesh%edges(m)%face_indices(2))%area
        do n=1,mesh%nedges
          Anmas = mesh%faces(mesh%edges(n)%face_indices(1))%area
          Anmenos = mesh%faces(mesh%edges(n)%face_indices(2))%area

          z1 = (mesh%edges(m)%length * mesh%edges(n)%length)/(Ammas*Anmas) *( intsmsn() - (1/k**2))*green()
          z2 = (mesh%edges(m)%length * mesh%edges(n)%length)/(Ammas*Anmenos) *( intsmsn() - (1/k**2))*green()
          z3 = (mesh%edges(m)%length * mesh%edges(n)%length)/(Ammenos*Anmas) *( intsmsn() - (1/k**2))*green()
          z4 = (mesh%edges(m)%length * mesh%edges(n)%length)/(Ammenos*Anmenos) *( intsmsn() - (1/k**2))*green()
        enddo

      end do

    end subroutine llenar_matriz
end module compute