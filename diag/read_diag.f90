program read_diag

!  ifort -o read_diag module_ncio.f90 read_diag.f90 -I/$NETCDF/include
!  -L/$NETCDF/lib -lnetcdff

   use module_ncio, only: ncio

   implicit none

   type(ncio) :: diag

   character*180 :: diagfile
   character*180 :: outfile

   integer :: i
   integer :: nobs
   character, allocatable :: var(:,:)
   character, allocatable :: stationID(:,:)
   integer, allocatable :: itype(:)
   real, allocatable :: rdhr(:)
   real, allocatable :: rlat(:)
   real, allocatable :: rlon(:)
   real, allocatable :: rprs(:)
   real, allocatable :: iuse(:)
   real, allocatable :: robs1(:)
   real, allocatable :: ddiff(:)
   integer, allocatable :: iuse_int(:)

   diagfile = '/scratch1/BMC/wrfruc/ejames/diag_conv_t_ges.2022111813.nc4'
   outfile = '/scratch1/BMC/wrfruc/ejames/diag_test.txt'
   call diag%open(trim(diagfile),'r',200)
   call diag%get_dim("nobs",nobs)
   write(*,*) 'nobs:', nobs

   allocate(var(7,nobs))
   allocate(stationID(8,nobs))
   allocate(itype(nobs))
   allocate(rdhr(nobs))
   allocate(rlat(nobs))
   allocate(rlon(nobs))
   allocate(rprs(nobs))
   allocate(iuse(nobs))
   allocate(robs1(nobs))
   allocate(ddiff(nobs))
   allocate(iuse_int(nobs))

   call diag%get_var_nc_char_2d("Observation_Class",7,nobs,var)
   call diag%get_var_nc_char_2d("Station_ID",8,nobs,stationID)
   call diag%get_var_nc_int_1d("Observation_Type",nobs,itype)
   call diag%get_var_nc_real_1d("Time",nobs,rdhr)
   call diag%get_var_nc_real_1d("Latitude",nobs,rlat)
   call diag%get_var_nc_real_1d("Longitude",nobs,rlon)
   call diag%get_var_nc_real_1d("Pressure",nobs,rprs)
   call diag%get_var_nc_real_1d("Analysis_Use_Flag",nobs,iuse)
   call diag%get_var_nc_real_1d("Observation",nobs,robs1)
   call diag%get_var_nc_real_1d("Obs_Minus_Forecast_adjusted",nobs,ddiff)

   open(42, file=trim(outfile))

   do i=1,nobs

      iuse_int(i) = int(iuse(i))   

      write (42,'(A3," @ ",A8," : ",I3,F6.2,F8.2,F8.2,F8.2,I3,2F8.2)') &
                   var(5,i)//var(6,i)//var(7,i),stationID(1,i)//stationID(2,i)//stationID(3,i)//stationID(4,i)//stationID(5,i)//stationID(6,i)//stationID(7,i)//stationID(8,i),itype(i),rdhr(i),rlat(i),rlon(i),rprs(i),iuse_int(i),robs1(i),ddiff(i)

   enddo

end program read_diag
