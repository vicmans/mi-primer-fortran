subroutine prodescalar_matriz(A,B,t)
real,dimension(:,:),intent(in)::A,B
real,intent(out)::t
t=sum(A*B)
end subroutine prodescalar_matriz
program demo11
real::A_punto_B
real,dimension(10,10)::A,B
interface
subroutine prodescalar_matriz(A,B,t)
real,dimension(:,:),intent(in)::A,B
real,intent(out)::t
end subroutine prodescalar_matriz
end interface
A=1.; B=1.
call prodescalar_matriz(A(1:3,1:3),B(1:5:2,1:3),A_punto_B)
write(*,*)'El producto escalar de A y B es',A_punto_B
end program demo11