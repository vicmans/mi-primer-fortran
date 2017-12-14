#!/bin/bash
echo "compilando"
gfortran -o prueba2 constants.f90 linalg.f90 mesh.f90 aux.f90 prueba2.f90
echo "Listo. Ejecute prueba2"
