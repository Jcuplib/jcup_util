#define TIME_STAMP 'Time-stamp: <2015/08/24 13:42:10 JST by Takahiro INOUE mod_tdc.F90>'
!> Top module for Time, Date and Calendar.
!!
!! This module is part of Jcup_util.
!!
!! \par ISO 8601 format
!! - basic    : character(len=20), `YYYYMMDDThhmmss+0900`
!! - extended : character(len=25), `YYYY-MM-DDThh:mm:ss+09:00`
!! .
!!
!! \par date_and_time format
!! - cdate  : character(len=8) , `YYYYMMDD`
!! - ctime  : character(len=10), `hhmmss.sss`
!! - czone  : character(len=5) , `+hhmm`
!! - vals   : integer,dimension(8), as follows:
!! .
!!
!!
!!   vals    |  Meaning
!!   --------|--------------------------------
!!   vals(1) |  The year
!!   vals(2) |  The month
!!   vals(3) |  The day of the month
!!   vals(4) |  Time difference with UTC in minutes
!!   vals(5) |  The hour of the day
!!   vals(6) |  The minutes of the hour
!!   vals(7) |  The seconds of the minute
!!   vals(8) |  The milliseconds of the second
!!
!!
!! \par calendar
!! - Ideal 365 days ( no leap )
!! - Proleptic Gregorian calendar
!! - (True) Gregorian
!! - Jurian
!! - Ideal 360 days
!! - Ideal XXX days ( for another planet )
!! .
!!
!! \todo
!! Should time difference be other class than timedate_t ??
!!
!!
!! \author Takahiro INOUE <tinoue@rist.jp>
!!
!! \date
!!  - 2015/05/01 (don)        Start Implementation
!!
!!
module mod_tdc
  use iso_fortran_env,only : output_unit

  use mod_tdc_calen_type
  use mod_tdc_timedate_type
  implicit none 

  private ! is default
  public :: timedate_t
  public :: tdc_secs_k !! bollowed from mod_tdc_calen_type

  public :: calen_ProGre, calen_ProGre_NL, calen_TruGre, calen_Jurian, calen_IdealM
  public :: form_iso_bas
  public :: form_iso_ext
  public :: form_czone
  public :: form_czone_ext

  public :: tdc_set_default_calen_type
  public :: tdc_conv_vals_to_tsecs
  public :: is_time_period_passed


  integer,parameter :: tdc_secs_k = secs_k

  character(len=80),private,save :: timestamp=TIME_STAMP

contains
  !!========================================================================
  !> Initialize this and sub modules
  !!
  subroutine tdc_init()


  end subroutine tdc_init


  !!========================================================================
  !> set default calendar type.
  !!
  subroutine tdc_set_default_calen_type( itype )
    integer,intent(in) :: itype !< calendar type, defined in mod_tdc_calen_type.

    call timedate_t_set_default_calen( itype )

    return

  end subroutine tdc_set_default_calen_type



  !!========================================================================
  !> Convert values to total second
  !!
  !! values NOT given are set as zero.
  !!
  !! If none of values is given, result will be mess.
  !!
  !!
  subroutine tdc_conv_vals_to_tsecs( tsecs, yr, mo, dy, hr, mn, sc, ms, df)
    integer(secs_k),intent(in),optional :: yr
    integer(secs_k),intent(in),optional :: mo
    integer(secs_k),intent(in),optional :: dy
    integer(secs_k),intent(in),optional :: hr
    integer(secs_k),intent(in),optional :: mn
    integer(secs_k),intent(in),optional :: sc
    integer(secs_k),intent(in),optional :: ms
    integer(secs_k),intent(in),optional :: df
    integer(secs_k),intent(out) :: tsecs !< total secs

    integer(secs_k) :: v(8)
    type(timedate_t) :: tt

    v(1:8) = int(0,secs_k)
    if ( present(yr) ) v(1) = yr
    if ( present(mo) ) v(2) = mo
    if ( present(dy) ) v(3) = dy
    if ( present(hr) ) v(4) = hr
    if ( present(mn) ) v(5) = mn
    if ( present(sc) ) v(6) = sc
    if ( present(ms) ) v(7) = ms
    if ( present(df) ) v(8) = df
    


    call tt%init()
    call tt%set_from_values(&
      &  v(1),v(2),v(3),&
      &  v(4),v(5),v(6),&
      &  v(7),v(8))

    tsecs= tt%get_tsecs()

    return

  end subroutine tdc_conv_vals_to_tsecs


  



  !!========================================================================
  !> Check if time period have passed.
  !!
  !! Return .true. if given `time` is passed the time period `tintv`
  !! from previous time `tprev`.
  !!
  !! `tunit` is a unit of tintv, and the first 2-letters are
  !! meaningful, such as 'ye'ar, 'mo'nth, 'da'ys, 'ho'ur, 'mi'n,
  !! 'se'cond.
  !!
  !!
  !! Time period begins from `torgn`. Even if `time` is later than `tprev`, 
  !! result is .false. when time and tprev is in the same time period.
  !!
  !! In the figure below,
  !! - if time==t0, result is .false. ,
  !! - if time==t1, result is .true. .
  !!
  !! \verbatim
  !!
  !!   torgn                          tprev   t0    t1
  !!    |                                /   /     /    
  !!    |--------+--------+--.......--+-^---^--+--^-----+-----....
  !!    |<------>|<------>|           |        |        |
  !!       tintv                         
  !!
  !! \endverbatim
  !!
  !!
  !! For `tunit` is year or month, comparing `time` and `tprev` by
  !! representing them in year or month with decimal fraction.
  !!
  !! \todo
  !! The case `tunit` being year or month is imitating the way of
  !! MIROC5, but don't know why these case are different from the case
  !! `tunit` is day and other. Or should modify to be the same way ?
  !!
  !!
  !!
  function is_time_period_passed( time, tprev, torgn, tintv, tunit ) result(res)
    integer(secs_k), intent(in) :: time !< current time to be checked.
    integer(secs_k), intent(in) :: tprev !< previous end of period.
    integer(secs_k), intent(in) :: torgn !< origin of period.
    integer(secs_k), intent(in) :: tintv !< time interval.
    character(len=*),intent(in) :: tunit !< unit of tintv.

    integer(secs_k) :: tintv0

    type(timedate_t) :: t1 !! convert from time
    type(timedate_t) :: t0 !! convert from tprev

    integer(secs_k) :: yr1, mo1, dy1 !! convert from time
    integer(secs_k) :: yr0, mo0, dy0 !! convert from tprev
    double precision  :: rt1 ! ratio of time in tunit
    double precision  :: rt0 ! ratio of tprev in tunit

    logical :: res !< .true. if time period has passed.


    !! Why ??
!!$    res = ( time == tprev )
!!$    if ( res ) return

    res = .false.
    
    if ( torgn < 0 ) call exit(1)
    if ( time < torgn .or. tprev < torgn ) return


    select case ( tunit(1:2) )
    case ( 'YE', 'Ye', 'ye', 'YR', 'Yr', 'yr' )
      call t0%init()
      call t0%set_tsecs( tprev )
      call t0%query_values( yr = yr0, mo = mo0, dy = dy0 )
      rt0 = yr0 + dble(mo0)/dble(t0%get_mo_yr()) + dble(dy0)/dble(t0%get_dy_yr())

      call t1%init()
      call t1%set_tsecs( time )
      call t1%query_values( yr = yr1, mo = mo1, dy = dy1 )
      rt1 = yr1 + dble(mo1)/dble(t1%get_mo_yr()) + dble(dy1)/dble(t1%get_dy_yr())

      res = ( (rt1-rt0) >=dble( tintv ) )

    case ( 'MO', 'Mo', 'mo' )
      call t0%init()
      call t0%set_tsecs( tprev )
      call t0%query_values( yr = yr0, mo = mo0, dy = dy0 )
      rt0 = dble(yr0 * t0%get_mo_yr()) + dble(mo0) + dble(dy0)/dble(t0%get_dy_yr())

      call t1%init()
      call t1%set_tsecs( time )
      call t1%query_values( yr = yr1, mo = mo1, dy = dy1 )
      rt1 = dble(yr1 * t1%get_mo_yr()) + dble(mo1) + dble(dy1)/dble(t1%get_dy_yr())

      res = ( (rt1-rt0) >=dble( tintv ) )

    case ( 'DA', 'Da', 'da', 'DY', 'Dy', 'dy' )
      call t1%init()
      call t1%set_tsecs( time )
      tintv0 = tintv * t1%get_sc_dy() !! seconds per day
      res = ( (time - torgn)/tintv0 > (tprev-torgn)/tintv0 )
    case ( 'HO', 'Ho', 'ho', 'HR', 'Hr', 'hr' )
      call t1%init()
      call t1%set_tsecs( time )
      tintv0 = tintv * 60 * 60  !! seconds per hour
      res = ( (time - torgn)/tintv0 > (tprev-torgn)/tintv0 )
    case ( 'MI', 'Mi', 'mi', 'MN', 'Mn', 'mn' )
      call t1%init()
      call t1%set_tsecs( time )
      tintv0 = tintv * 60       !! seconds per minute
      res = ( (time - torgn)/tintv0 > (tprev-torgn)/tintv0 )
    case ( 'SE', 'Se', 'se', 'SC', 'Sc', 'sc' )
      call t1%init()
      call t1%set_tsecs( time )
      res = ( (time - torgn)/tintv > (tprev-torgn)/tintv )
      !! \todo how about milli-second ??
    case default
      !! \todo error handling
      call exit(1)
    end select

  end function is_time_period_passed



end module mod_tdc




!!$=======================================================================
#ifdef TEST
!!$=======================================================================
program test_tdc

  use mod_tdc
  implicit none

  integer :: itest
  character(len=2) :: ctest
  integer :: itype
  character(len=2) :: ctype


  type(timedate_t) :: tt, t1, t2
  integer(tdc_secs_k) :: td
  character(len=4) :: tunit='sc'

  character(len=8) :: cdate = '20150520'
  character(len=10):: ctime = '120546'
  character(len=5) :: czone = 'Z'
  integer(tdc_secs_k) :: vals(8) = [2015,05,20,12,05,46,0,0]

  integer(tdc_secs_k) :: ts
  


  integer :: n
  
  itest = 1
  itype = 0
  n = command_argument_count()
  select case ( n ) 
  case ( :0 )
    !! default
  case ( 1 )
    call get_command_argument(1,ctest)
    read(ctest,*) itest
  case ( 2 )
    call get_command_argument(1,ctest)
    read(ctest,*) itest
    call get_command_argument(2,ctype)
    read(ctype,*) itype
  end select
  write(*,'(A,I0)')'itest: ',itest
  write(*,'(A,I0)')'itype: ',itype



  select case ( itype )
  case ( 1 )
    call tdc_set_default_calen_type( calen_ProGre )
  case ( 2 )
    call tdc_set_default_calen_type( calen_ProGre_NL )
  case ( 3 ) 
    call tdc_set_default_calen_type( calen_IdealM )
!!$  case ( 4 ) 
!!$    call tdc_set_default_calen_type( calen_ProGre_NL )
  case default
    !! use default
  end select

  call t1%init()
  call t1%set_from_strings(cdate=cdate,ctime=ctime)
  call t1%dump()


  select case ( itest )
  case ( 1 )
    td = 5400 ! implicit cast to integer(secs_k)
    tunit = 'sc'
    t2 = t1
    call t1%add_w_unit(int(-6,tdc_secs_k),tunit)
    call t2%add_w_unit(int(12800,tdc_secs_k),tunit)
    tt = t2

    write(*,*)'tintv:',td, tunit
    write(*,*)'torgn:',t1%format_iso_bas()
    write(*,*)'tprev:',t2%format_iso_bas()

    write(*,*)
    write(*,*)'time :',tt%format_iso_bas()
    write(*,*)'res:',is_time_period_passed(tt%get_tsecs(), t2%get_tsecs(), t1%get_tsecs(), td, tunit)
    call tt%add_w_unit(int(1000,tdc_secs_k),tunit)
    write(*,*)'time :',tt%format_iso_bas()
    write(*,*)'res:',is_time_period_passed(tt%get_tsecs(), t2%get_tsecs(), t1%get_tsecs(), td, tunit)
    call tt%add_w_unit(int(5000,tdc_secs_k),tunit)
    write(*,*)'time :',tt%format_iso_bas()
    write(*,*)'res:',is_time_period_passed(tt%get_tsecs(), t2%get_tsecs(), t1%get_tsecs(), td, tunit)
  case ( 2 )
    td = 540 ! implicit cast to integer(tdc_secs_k)
    tunit = 'min'
!!$    tunit = 'mn'
    t2 = t1
    call t1%add_w_unit(int(-6,tdc_secs_k),tunit)
    call t2%add_w_unit(int(1280,tdc_secs_k),tunit)
    tt = t2

    write(*,*)'tintv:',td, tunit
    write(*,*)'torgn:',t1%format_iso_bas()
    write(*,*)'tprev:',t2%format_iso_bas()

    write(*,*)
    write(*,*)'time :',tt%format_iso_bas()
    write(*,*)'res:',is_time_period_passed(tt%get_tsecs(), t2%get_tsecs(), t1%get_tsecs(), td, tunit)
    call tt%add_w_unit(int(100,tdc_secs_k),tunit)
    write(*,*)'time :',tt%format_iso_bas()
    write(*,*)'res:',is_time_period_passed(tt%get_tsecs(), t2%get_tsecs(), t1%get_tsecs(), td, tunit)
    call tt%add_w_unit(int(5000,tdc_secs_k),tunit)
    write(*,*)'time :',tt%format_iso_bas()
    write(*,*)'res:',is_time_period_passed(tt%get_tsecs(), t2%get_tsecs(), t1%get_tsecs(), td, tunit)

  case ( 3 )
    td = 54 ! implicit cast to integer(tdc_secs_k)
    tunit = 'hour'
!!$    tunit = 'hr'
    t2 = t1
    call t1%add_w_unit(int(-6,tdc_secs_k),tunit)
    call t2%add_w_unit(int(1280,tdc_secs_k),tunit)
    tt = t2

    write(*,*)'tintv:',td, tunit
    write(*,*)'torgn:',t1%format_iso_bas()
    write(*,*)'tprev:',t2%format_iso_bas()

    write(*,*)
    write(*,*)'time :',tt%format_iso_bas()
    write(*,*)'res:',is_time_period_passed(tt%get_tsecs(), t2%get_tsecs(), t1%get_tsecs(), td, tunit)
    call tt%add_w_unit(int(1,tdc_secs_k),tunit)
    write(*,*)'time :',tt%format_iso_bas()
    write(*,*)'res:',is_time_period_passed(tt%get_tsecs(), t2%get_tsecs(), t1%get_tsecs(), td, tunit)
    call tt%add_w_unit(int(120,tdc_secs_k),tunit)
    write(*,*)'time :',tt%format_iso_bas()
    write(*,*)'res:',is_time_period_passed(tt%get_tsecs(), t2%get_tsecs(), t1%get_tsecs(), td, tunit)


  case ( 4 )
    td = 14 ! implicit cast to integer(tdc_secs_k)
!!$    tunit = 'day'
    tunit = 'dy'
    t2 = t1
    call t1%add_w_unit(int(-6,tdc_secs_k),tunit)
    call t2%add_w_unit(int(1280,tdc_secs_k),tunit)
    tt = t2

    write(*,*)'tintv:',td, tunit
    write(*,*)'torgn:',t1%format_iso_bas()
    write(*,*)'tprev:',t2%format_iso_bas()

    write(*,*)
    write(*,*)'time :',tt%format_iso_bas()
    write(*,*)'res:',is_time_period_passed(tt%get_tsecs(), t2%get_tsecs(), t1%get_tsecs(), td, tunit)
    call tt%add_w_unit(int(1,tdc_secs_k),tunit)
    write(*,*)'time :',tt%format_iso_bas()
    write(*,*)'res:',is_time_period_passed(tt%get_tsecs(), t2%get_tsecs(), t1%get_tsecs(), td, tunit)
    call tt%add_w_unit(int(120,tdc_secs_k),tunit)
    write(*,*)'time :',tt%format_iso_bas()
    write(*,*)'res:',is_time_period_passed(tt%get_tsecs(), t2%get_tsecs(), t1%get_tsecs(), td, tunit)


  case ( 5 )
    td = 2 ! implicit cast to integer(tdc_secs_k)
    tunit = 'mon'

    call t1%init()
    call t1%set_from_strings(cdate='2014-06-30', ctime='00:00:00')
    call t2%init()
    call t2%set_from_strings(cdate='2016-06-15', ctime='00:00:00')
    write(*,*)'tintv:',td, tunit
    write(*,*)'torgn:',t1%format_iso_bas()
    write(*,*)'tprev:',t2%format_iso_bas()
    write(*,*)

    tt = t2
    call tt%set_from_strings(cdate='2016-07-15')
    write(*,*)'time :',tt%format_iso_bas()
    write(*,*)'res:',is_time_period_passed(tt%get_tsecs(), t2%get_tsecs(), t1%get_tsecs(), td, tunit)

    call tt%set_from_strings(cdate='2016-08-15')
    write(*,*)'time :',tt%format_iso_bas()
    write(*,*)'res:',is_time_period_passed(tt%get_tsecs(), t2%get_tsecs(), t1%get_tsecs(), td, tunit)


    call tt%set_from_strings(cdate='2016-09-15')
    write(*,*)'time :',tt%format_iso_bas()
    write(*,*)'res:',is_time_period_passed(tt%get_tsecs(), t2%get_tsecs(), t1%get_tsecs(), td, tunit)


  case ( 6 )
    td = 1 ! implicit cast to integer(tdc_secs_k)
    tunit = 'year'

    call t1%init()
    call t1%set_from_strings(cdate='2014-06-30', ctime='00:00:00')
    call t2%init()
    call t2%set_from_strings(cdate='2016-05-15', ctime='00:00:00')

    tt = t2

    write(*,*)'tintv:',td, tunit
    write(*,*)'torgn:',t1%format_iso_bas()
    write(*,*)'tprev:',t2%format_iso_bas()

    write(*,*)
    call tt%set_from_strings(cdate='2016-07-15')
    write(*,*)'time :',tt%format_iso_bas()
    write(*,*)'res:',is_time_period_passed(tt%get_tsecs(), t2%get_tsecs(), t1%get_tsecs(), td, tunit)

    call tt%set_from_strings(cdate='2017-06-15')
    write(*,*)'time :',tt%format_iso_bas()
    write(*,*)'res:',is_time_period_passed(tt%get_tsecs(), t2%get_tsecs(), t1%get_tsecs(), td, tunit)

    call tt%set_from_strings(cdate='2017-07-15')
    write(*,*)'time :',tt%format_iso_bas()
    write(*,*)'res:',is_time_period_passed(tt%get_tsecs(), t2%get_tsecs(), t1%get_tsecs(), td, tunit)


  case ( 11 ) 
    call tdc_conv_vals_to_tsecs(ts, &
      & vals(1), vals(2), vals(3),&
      & vals(4), vals(5), vals(6) )
    write(*,'(A)')'test tdc_conv_vals_to_tsecs:'
    write(*,'(A10,8I6)')'vals:',vals
    write(*,'(A10,I0)')'tsecs:',ts
    
  case default
    call exit(1)
  end select

end program test_tdc
!!$=======================================================================
#endif


!!$=======================================================================
!!$ local variables:
!!$ mode: f90
!!$ abbrev-mode: t
!!$ f90-beginning-ampersand: t
!!$ f90-do-indent: 2
!!$ f90-if-indent: 2
!!$ f90-program-indent: 2
!!$ f90-type-indent: 2
!!$ f90-continuation-indent: 2
!!$ f90-indented-comment-re: "![\>!|!]"
!!$ time-stamp-format: "%:y/%02m/%02d %02H:%02M:%02S %Z by %U %f"
!!$ end:
  
