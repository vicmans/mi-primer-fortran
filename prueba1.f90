program prueba1
 use mesh
 ! prueba leyendo un archivo mesh .msh
 TYPE(mesh_container) :: meshs
 meshs = load_mesh('sphere.msh')
 print *, 'Ahora vamos a esportarlo'
 
 call export_mesh('nuevo.msh',meshs)
 print *, 'Fino, ojala aiga servido'
end program prueba1