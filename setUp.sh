#!/bin/bash

echo "🔧 Configurando el proyecto..."

# Instalar dependencias
cabal update
cabal build

echo "✅ Proyecto listo. Para ejecutarlo, usa: ./run.sh"
