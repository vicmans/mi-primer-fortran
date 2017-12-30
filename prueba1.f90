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
print*,'CARAS'
 do i=1,meshs%nfaces
   print *, meshs%faces(i)%node_indices
 enddo

 print *, 'Numero de edges', meshs%nedges
 print *, 'tamañe arreglo de edges', size(meshs%edges)
 
 print *, 'que peo, obteniebto los lados'
 call get_edges(meshs)
 print *, 'Listo, viendo los resultados'
 print *, 'dsdes de get edges, Numero de edges', meshs%nedges
 print *, 'tamañe arreglo de edges', size(meshs%edges)

 do i=1,meshs%nedges
   print *, i, 'edge ', meshs%edges(i)%bnode_indices
 enddo

 call export_mesh('nuevo.msh',meshs)
 print *, 'Fino, ojala aiga servido, vea el archivo nuevo.msh'
end program prueba1