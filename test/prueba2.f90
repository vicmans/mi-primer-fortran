program prueba2
 use mesh
 ! prueba leyendo un archivo mesh .msh
 TYPE(mesh_container) :: meshs
 Character(len=40) :: file
 REAL (KIND=dp) :: scale

 print *, 'Nombre del archivo'
 read *, file
 meshs = load_mesh(file)

 print *, 'Ahora vamos a esportarlo, usando build mesh'
 
 !call export_mesh('nuevo.msh',meshs)
 ! meshs es la variables con los mesh
 scale = 1.0
 call build_mesh(meshs,scale)

 print *, 'Fino, ojala aiga servido'
end program prueba2