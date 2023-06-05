#!/bin/bash

pdfDir="pdfs"
outputDir="pdfs-protected"
ownerPassword="cma2023"

mkdir -p "$outputDir"

find "$pdfDir" -type f -name "*.pdf" | while read -r file; do
  relativePath="${file#$pdfDir/}"
  outputFile="$outputDir/$relativePath"
  outputSubDir=$(dirname "$outputFile")
  
  mkdir -p "$outputSubDir"
  pdftk "$file" output "$outputFile" encrypt_128bit owner_pw "$ownerPassword"
done

echo "All PDF files in $pdfDir have been content-protected."
