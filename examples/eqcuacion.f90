program eqcuacion
	implicit none
	real :: a, b
	complex :: x1, x2
	! Progrma pa resolve la eq cuadratica
	! de 2do grado
	print *, 'Esto resuelve x^2 + ax + b = 0'
	print *, 'Introduzca a y b'
	print *, 'Introduzca a: '
	read *, a
	print *, 'Introduzca b: '
	read *, b
	! resultados
	x1 = (-a + sqrt(a**2 - 4*b))/2
	x2 = (-a - sqrt(a**2 - 4*b))/2

	print *, 'Los resultados son:'
	print *, x1
	print *, x2
end program eqcuacion