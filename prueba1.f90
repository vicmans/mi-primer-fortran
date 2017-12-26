program prueba1
 use mesh
 ! prueba leyendo un archivo mesh .msh
 TYPE(mesh_container) :: meshs
 CHARACTER(LEN=40) :: ar
 print *, 'Introduzca el archivo .msh que quiere leer'
 read *, ar

 call load_mesh(ar, meshs)
 print *, 'Ahora vamos a esportarlo'
 print *, 'Son', meshs%nnodes, 'nodes'

 do i=1,meshs%nnodes
   print *, meshs%nodes(i)%p
 enddo
 
 print *, 'Numero de edges', meshs%nedges
 

 call get_edges(meshs)

 call export_mesh('nuevo.msh',meshs)
 print *, 'Fino, ojala aiga servido, vea el archivo nuevo.msh'
end program prueba1