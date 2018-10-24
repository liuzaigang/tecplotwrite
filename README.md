# tecplotwrite
Fortran module used to write tecplot binary or text format data file

Usage:
   1. Define data structure
      type(tecplotDataTemplate) :: data2Write
   2. Specify the data to write
      *(use % to access data structure)
      integer :: inunit
           - Unit of data file to Write
      character(len=128)  :: datatitle
           - Filename and title of data file to Write 
              ( Extensions .plt or .dat will not be generated automatically. Please define. )
      integer :: Nvars
           - Number of variables
      character(len=string_length), dimension(:), pointer  :: varNames
           - Names of variables, dimension: 1:Nvars
      integer :: IMax
      integer :: JMax
      integer :: KMax
           - Dimension of the grid
      real(SP), dimension(:,:,:,:), pointer :: data
           - data to write, single precesion, dimension: 1:IMax,1:JMax,1:KMax,1:Nvars
   3. Write
      call tecplot_write_binary(data2Write) 
           - to write binary data (.plt files)
      call tecplot_write_ascii(data2Write)
           - to write text data (.dat files)
Done!
