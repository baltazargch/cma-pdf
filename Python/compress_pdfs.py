import os
from PyPDF2 import PdfReader, PdfWriter

# Define the input and output folders
input_folder = r'D:\Research\In process\cma-pdf\pdfs'	
output_folder = r'D:\Research\In process\cma-pdf\compress_pdfs'

# Define the compression level (0 = no compression, 9 = maximum compression)
compression_level = 7

# Loop through all PDF files in the input folder
for filename in os.listdir(input_folder):
    if filename.endswith('.pdf'):
        # Create a PDF reader object
        input_pdf = PdfReader(open(os.path.join(input_folder, filename), 'rb'))
        
        input_pdf.flattened_pages
        
        # Create a PDF writer object
        output_pdf = PdfWriter()

        # Loop through all pages in the input PDF
        for page in input_pdf.pages:
            # Add each page to the output PDF
            page.compress_content_streams() # This is CPU intensive!
            output_pdf.add_page(page)

        # Save the output PDF to the output folder with the same name as the input file
        with open(os.path.join(output_folder, filename), 'wb') as f:
            output_pdf.write(f)

# Print a message to indicate the batch process has completed
print("PDF compression complete!")
