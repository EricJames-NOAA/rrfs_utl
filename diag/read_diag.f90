program read_diag

!  ifort -o read_diag module_ncio.f90 read_diag.f90 -I/$NETCDF/include
!  -L/$NETCDF/lib -lnetcdff

   use module_ncio, only: ncio

   implicit none

   type(ncio) :: diag

   logical :: exists

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
   real, allocatable :: robs2(:)
   real, allocatable :: rdpt2(:)
   integer, allocatable :: iuse_int(:)

   CALL GETARG(1, diagfile)
   if (diagfile(90:91) .eq. 't_' .or. diagfile(90:91) .eq. 'q_') then
      outfile = '/lfs4/BMC/nrtrr/NCO_dirs/stmp/tmpnwprd/RRFS_conus_3km/2022112815/anal_conv_gsi/diag_results.conv_' // diagfile(92:94)
   else if (diagfile(90:91) .eq. 'ps' .or. diagfile(90:91) .eq. 'pw' .or. diagfile(90:91) .eq. 'rw' .or. diagfile(90:91) .eq. 'uv') then
      outfile = '/lfs4/BMC/nrtrr/NCO_dirs/stmp/tmpnwprd/RRFS_conus_3km/2022112815/anal_conv_gsi/diag_results.conv_' // diagfile(93:95)
   else
      outfile = '/lfs4/BMC/nrtrr/NCO_dirs/stmp/tmpnwprd/RRFS_conus_3km/2022112815/anal_conv_gsi/diag_results.conv_' // diagfile(94:96)
   endif

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
   allocate(robs2(nobs))
   allocate(rdpt2(nobs))
   allocate(iuse_int(nobs))

   call diag%get_var_nc_char_2d("Observation_Class",7,nobs,var)
   call diag%get_var_nc_char_2d("Station_ID",8,nobs,stationID)
   call diag%get_var_nc_int_1d("Observation_Type",nobs,itype)
   call diag%get_var_nc_real_1d("Time",nobs,rdhr)
   call diag%get_var_nc_real_1d("Latitude",nobs,rlat)
   call diag%get_var_nc_real_1d("Longitude",nobs,rlon)
   call diag%get_var_nc_real_1d("Pressure",nobs,rprs)
   call diag%get_var_nc_real_1d("Analysis_Use_Flag",nobs,iuse)
   if (diagfile(90:91) .eq. 'uv') then
      call diag%get_var_nc_real_1d("u_Observation",nobs,robs1)
      call diag%get_var_nc_real_1d("u_Obs_Minus_Forecast_adjusted",nobs,ddiff)
      call diag%get_var_nc_real_1d("v_Observation",nobs,robs2)
      call diag%get_var_nc_real_1d("v_Obs_Minus_Forecast_adjusted",nobs,rdpt2)
   else
      call diag%get_var_nc_real_1d("Observation",nobs,robs1)
      call diag%get_var_nc_real_1d("Obs_Minus_Forecast_adjusted",nobs,ddiff)
   endif

   inquire(file=trim(outfile), exist=exists)
   if (exists) then
      open(42, file=trim(outfile), status='old', position='append', action='write')
   else
      open(42, file=trim(outfile), status='new', action='write')
   endif

   do i=1,nobs

      iuse_int(i) = int(iuse(i))   

      if (diagfile(90:91) .eq. 'uv') then

         write (42,'(A3," @ ",A8," : ",I3,F6.2,F8.2,F8.2,F8.2,I3,4F8.2)') &
                   var(5,i)//var(6,i)//var(7,i),stationID(1,i)//stationID(2,i)//stationID(3,i)//stationID(4,i)//stationID(5,i)//stationID(6,i)//stationID(7,i)//stationID(8,i),itype(i),rdhr(i),rlat(i),rlon(i),rprs(i),iuse_int(i),robs1(i),ddiff(i),robs2(i),rdpt2(i) 

      else

         write (42,'(A3," @ ",A8," : ",I3,F6.2,F8.2,F8.2,F8.2,I3,2F8.2)') &
                   var(5,i)//var(6,i)//var(7,i),stationID(1,i)//stationID(2,i)//stationID(3,i)//stationID(4,i)//stationID(5,i)//stationID(6,i)//stationID(7,i)//stationID(8,i),itype(i),rdhr(i),rlat(i),rlon(i),rprs(i),iuse_int(i),robs1(i),ddiff(i)

      endif

   enddo

   print *, diagfile(90:91)

end program read_diag
