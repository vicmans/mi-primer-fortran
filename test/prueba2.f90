program prueba2
 use mesh
 ! prueba leyendo un archivo mesh .msh
 TYPE(mesh_container) :: meshs
 Character(len=40) :: file
 print *, 'Nombre del archivo'
 read *, file
 meshs = load_mesh(file)

 print *, 'Ahora vamos a esportarlo'
 
 call export_mesh('nuevo.msh',meshs)
 ! meshs es la variables con los mesh

 !call build_solid_faces(meshs)

 print *, 'Fino, ojala aiga servido'
end program prueba2