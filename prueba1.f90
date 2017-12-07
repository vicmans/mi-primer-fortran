program prueba1
 use mesh
 ! prueba leyendo un archivo mesh .msh
 TYPE(mesh_container) :: meshs
 CHARACTER(LEN=40) :: ar
 print *, 'Introduzca el archivo .msh que quiere leer'
 read *, ar

 meshs = load_mesh(ar)
 print *, 'Ahora vamos a esportarlo'
 
 call export_mesh('nuevo.msh',meshs)
 print *, 'Fino, ojala aiga servido, vea el archivo nuevo.msh'
end program prueba1