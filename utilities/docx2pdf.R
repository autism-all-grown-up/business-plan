library(officer)
library(RDCOMClient)

# Define the input DOCX and output PDF file paths
input_docx <- "../_book/Autism-All-Grown-Up.docx"
output_pdf <- "../_book/Autism-All-Grown-Up.pdf"

# Open a COM connection to Word
word_app <- COMCreate("Word.Application")
word_app[["Visible"]] <- TRUE

# Open the DOCX file
doc <- word_app[["Documents"]]$Open(normalizePath(input_docx))

# Save the document as PDF
doc$SaveAs2(FileName = normalizePath(output_pdf), FileFormat = 17) # wdFormatPDF = 17

# Close the document and quit Word
doc$Close()
word_app$Quit()

