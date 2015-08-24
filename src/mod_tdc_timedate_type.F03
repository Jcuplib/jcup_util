#define TIME_STAMP 'Time-stamp: <2015/08/24 14:00:52 JST by Takahiro INOUE mod_tdc_timedate_type.F03>'
!> Module to define timedate_t class, sub module of mod_tdc.
!!
!! `timedate_t` class manages Time/Date.
!!
!! Time/Date are represented and held as 8 integer
!! values like date_and_time(), that is;
!! - year
!! - month 
!! - day   
!! - hour  
!! - minute 
!! - second 
!! - milli-second 
!! - timezone
!! .
!!
!! Milli-second is optional. Timezone is represented as a difference
!! from UTC in minutes. For example, timezone of Tokyo(+0900) is +540,
!! as the same with date_and_time().
!!
!! \par Unit Strings
!!
!! Some methods take `unit` strings as an argument. The first
!! 2-letters of it are meaningful, such as 'ye'ars, 'mo'nths, 'da'ys,
!! 'ho'urs, 'mi'nutes, 'se'conds, and 'ms'econds. Or, you can use
!! 2-letters acronyms, i.e. `yr', 'mo', 'dy', 'hr', 'mn', 'sc' (and
!! 'ms' as well).
!! 
!!
!!
!! \par Calendar.
!!
!! For conventional calendar, one year is 12 months and 365 days
!! unless leap year.  You can use another calendar that has, for
!! example, 360 days and 12 months per year and each month equally
!! have 30 days.
!!
!! On the other hand, conversion hr:mn:sc from/to secs are straight
!! forward, because seconds per minute and minutes per hour are fixed
!! as 60 sec/min and 60 min/hour, respectively.
!!
!! But the length of the day is also dependent on the calendar.
!! For normal calendar, one day is 86400 seconds, but, for example,
!! one day on Mars is about 88775 seconds.
!!
!! So this class has a member of extended class of type(calen_t),
!! defined in mod_tdc_calen_type.  Conversion of time/date is done by
!! methods of this class. See mod_tdc_calen_type for details and
!! available calendars.
!!
!!
!! \par Time/Date Expression Strings.
!!
!! This class has many methods ("type bound procedures" technically),
!! mainly for set/get time/date in various form, such as;
!! - integer values
!! - strings in ISO 8601 basic/extended format
!! - strings in date_and_time() argument format (calls cdate/ctime/czone respectively).
!! .
!!
!! For ISO 8601 format, representation in this class are as below.
!!
!!  format                   | representation            | example
!!  -------------------------|---------------------------|----------------------------
!!  basic w/Calendar dates   | yyyymmddThhmmss+hhmm      | 20150513T112834+0900
!!  basic w/Ordinal dates    | yyyydddThhmmss+hhmm       | 2015121T112834+0900
!!  extended w/Calendar dates| yyyy-mm-ddThh:mm:ss+hh:mm | 2015-05-13T11:28:34+09:00
!!  extended w/Ordinal dates | yyyy-dddThh:mm:ss+hh:mm   | 2015-121T11:28:34+09:00
!!
!!
!! Note that the letter 'T' is a delimiter between date and time.
!!
!! `ddd` in Ordinal dates is days in the year.
!!  `ss` may be `ss.sss` if milli-second is used.
!!  Timezone part may be `Z` for UTC.
!!
!! date_and_time() argument format is the same with date/time/timezone
!! part of ISO8601 basic format above, and that of extended format are allowed.
!!
!! Note that in these format, year must be in four digit,
!! month/day/hour/minute/second must be two digit and days of the year
!! in ordinal dates and milli-second must be up to 3 digits.
!!
!! On setting time/date, values may have any digits or even negative
!! value as far as they are meaningful in the calendar in use, they will
!! be "normalized" internally.
!!
!! \par Internal
!!
!! This class has 8 integer members for representing time/date, as follows:
!! - year : `yr`
!! - month : `mo`
!! - day   : `dy`
!! - hour  : `hr`
!! - minute : `mn`
!! - second : `sc`
!! - milli-second : `ms`
!! - timezone : `df`
!! .
!!
!! This class has another integer members below;
!! - `dys` : total days from epoch
!! - `scs` : total seconds in the day
!! - `tscs` : total seconds from epoch
!! .
!!
!! At present, epoch is 0001/01/01. Setting epoch freely is under
!! development.
!!
!! `days`/`secs`/`tsecs` and 8 integer member values are converted mutually
!! and instantly as the other touched in each methods.
!!
!! Internal arithmetic operations of time/date are done in
!! `days`/`secs`/`tsecs`.
!! 
!! Conversion yr-mo-dy from/to days is dependent on the calendar the
!! instance of this class uses.
!!
!! \todo
!! - Handling of milli-second is imcomplete.
!! - Handling of unit strings is not acomplished.
!!
!! This module is implemented from scratch.
!!
!! \copyright (c)2015 RIST
!!
!! \author Takahiro INOUE <tinoue@rist.jp>
!!
!! \date
!!  - 2015/05/01 (don)        Start Implementation
!!

module mod_tdc_timedate_type
  use iso_fortran_env, only : output_unit
  use mod_tdc_calen_type
  implicit none

  private ! is default
  public :: timedate_t
  public :: form_iso_bas
  public :: form_iso_ext
  public :: form_czone
  public :: form_czone_ext

  public :: timedate_t_set_default_calen

  !! from mod_tdc_calen_type
  public :: calen_ProGre, calen_ProGre_NL, calen_TruGre, calen_Jurian, calen_IdealM
  
!!$  integer,parameter,public :: secs_k = 8 !! tentative
  public :: secs_k !! bollowed from mod_tdc_calen_type

  !> Class for storing time/date, defined in mod_tdc_timedate_type.
  !!
  !! At present, epoch is 0001/01/01T00:00:00Z
  type timedate_t
    private
    integer(secs_k) :: yr   = 0 !< year
    integer(secs_k) :: mo   = 0 !< month
    integer(secs_k) :: dy   = 0 !< day
    integer(secs_k) :: hr   = 0 !< hour
    integer(secs_k) :: mn   = 0 !< minute
    integer(secs_k) :: sc   = 0 !< second
    integer(secs_k) :: ms   = 0 !< milli-second
    integer(secs_k) :: df   = 0 !< difference from UTC in minutes 
    integer(secs_k) :: dys  = 0 !< total days from epoch
    integer(secs_k) :: scs  = 0 !< total seconds in the day
    integer(secs_k) :: tscs = 0 !< total seconds from epoch
    class(calen_t),allocatable :: cal 
  contains
    procedure :: init  => timedate_t_init         !< set calendar type and initialize
    procedure :: dump  => timedate_t_dump         !< dump members
    !!
    procedure :: set_days         => timedate_t_set_days         !< set days then yr-mo-dy
    procedure :: set_secs         => timedate_t_set_secs         !< set secs then hr-mn-sc
    procedure :: set_tsecs        => timedate_t_set_tsecs        !< set total secs
    procedure :: set_daysecs      => timedate_t_set_daysecs      !< set days and secs simultaneously
    procedure :: set_from_values  => timedate_t_set_from_values  !< set members
    procedure :: set_from_strings => timedate_t_set_from_strings !< set members
    procedure :: set_w_unit       => timedate_t_set_w_unit       !< set value with unit
    !!
    procedure :: get_values => timedate_t_get_values        !< return values(8) array
    procedure :: get_days   => timedate_t_get_days        !< return days
    procedure :: get_secs   => timedate_t_get_secs        !< return secs
    procedure :: get_tsecs  => timedate_t_get_tsecs        !< return total secs
    procedure :: query_values => timedate_t_query_values !< query specivied values
    !!
    procedure :: get_dy_yr  => timedate_t_get_dy_yr       !< return days/year of the time.
    procedure :: get_mo_yr  => timedate_t_get_mo_yr       !< retunr months/year of the time.
    procedure :: get_sc_dy  => timedate_t_get_sc_dy       !< return secs/day of the time.
    !!
    procedure :: format_iso_bas => timedate_t_format_iso_bas  !< format in iso8601 basic
    procedure :: format_iso_ext => timedate_t_format_iso_ext  !< format in iso8601 extended
    procedure :: format_cdate   => timedate_t_format_cdate  !< format in date_and_time format strings
    procedure :: format_ctime   => timedate_t_format_ctime  !< format in date_and_time format strings
    procedure :: format_czone   => timedate_t_format_czone  !< format in date_and_time format strings
    !!
    procedure :: add_days   => timedate_t_add_days   !< Add days
    procedure :: add_secs   => timedate_t_add_secs   !< Add secs
    procedure :: add_tsecs  => timedate_t_add_tsecs  !< Add total secs
    procedure :: add_values => timedate_t_add_values !< Add values
    procedure :: add_w_unit => timedate_t_add_w_unit !< Add value w/ unit.
    !!
    procedure :: sub => timedate_t_sub !< Duration of `other` from `self`, return [days,secs]
    procedure :: shift_tz => timedate_t_shift_tz
!!$    procedure :: normalize => timedate_t_normalize
    !!
!!$    generic :: assignment(=) => timedate_t_alloc_assgn, timedate_t_copy
  end type timedate_t

  character(len=*),parameter :: form_iso_bas  = '(I4.4,I2.2,I2.2,"T",I2.2,I2.2,I2.2)' ! for write
  character(len=*),parameter :: form_iso_bas0 = '(I4.4,I2.2,I2.2,1X ,I2.2,I2.2,I2.2)' ! for read
  character(len=*),parameter :: form_iso_ext  = '(I4.4,"-",I2.2,"-",I2.2,"T",I2.2,":",I2.2,":",I2.2)' ! for write
  character(len=*),parameter :: form_iso_ext0 = '(I4.4,1X ,I2.2,1X ,I2.2,1X ,I2.2,1X ,I2.2,1X ,I2.2)' ! for read

  character(len=*),parameter :: form_cdate   = '(I4.4,I2.2,I2.2)'
  character(len=*),parameter :: form_cdate1  = '(I4.4,I2.2,I2.2)'        !! YYYYMMDD
  character(len=*),parameter :: form_cdate2  = '(I4.4,1X,I2.2,1XI2.2)'   !! YYYY-MM-DD

  character(len=*),parameter :: form_ctime  = '(I2.2,I2.2,I2.2,".",I3.3)'
  character(len=*),parameter :: form_ctime0  = '(I2.2,I2.2,I2.2,1X,I3.3)'
  character(len=*),parameter :: form_ctime1  = '(I2.2,I2.2,I2.2)'               !! hhmmss
  character(len=*),parameter :: form_ctime2  = '(I2.2,1X,I2.2,1X,I2.2)'         !! hh:mm:ss
  character(len=*),parameter :: form_ctime3  = '(I2.2,I2.2,I2.2,1X,I3.3)'       !! hhmmss.sss
  character(len=*),parameter :: form_ctime4  = '(I2.2,1X,I2.2,1X,I2.2,1X,I3.3)' !! hh:mm:ss.sss

  character(len=*),parameter :: form_czone       = '(SP,I3.2,SS,I2.2)'
  character(len=*),parameter :: form_czone_ext   = '(SP,I3.2,":",SS,I2.2)'
  character(len=*),parameter :: form_czone_ext0  = '(SP,I3.2,1X,SS,I2.2)'


  integer,save :: calen_default = calen_ProGre !< default calendar type.

  character(len=90),private,save :: timestamp=TIME_STAMP


contains
  !!========================================================================
  !> Set calendar type and (re)initialize the instance.
  !!
  !! To set calendar, you have to call this method explicitly.
  !!
  !! When `itype` is omitted `calen_default` is assumed.
  !!
  !! `calen_default` can be changed by timedate_t_set_default_calen().
  !!
  !! This class has an member with allocatable attribute, you cannot
  !! use automatic initialization.
  !!
  subroutine timedate_t_init( self, itype )
    class(timedate_t),intent(inout) :: self !< timedate_t instance
    integer ,intent(in), optional :: itype !< calendar type specifier.

    !! note that every member is secs_k.
    self%yr = 0
    self%mo = 0
    self%dy = 0
    self%hr = 0
    self%mn = 0
    self%sc = 0
    self%ms = 0
    self%df = 0
    self%dys = 0
    self%scs = 0
    self%tscs= 0

    if ( present(itype) ) then
      call calen_t_set_type(self%cal,itype)
    else
      call calen_t_set_type(self%cal,calen_default)
    end if

    call self%cal%init()

    return

  end subroutine timedate_t_init



  !!========================================================================
  !> Dump members
  !!
  !! When `lun` is given, output to that instead of `output_unit` in
  !! iso_fortran_env.
  !!
  !! When `header` is given, appended to the first line.
  !! 
  subroutine timedate_t_dump(self,header,lun)
    class(timedate_t),intent(in) :: self !< timedate_t instance
    character(len=*) ,intent(in),optional :: header !< header for prettyprint
    integer          ,intent(in),optional :: lun    !< logical unit number to output

    integer :: lun0

    if ( present(lun) ) then
      lun0 = lun
    else
      lun0 = output_unit
    end if

    if ( present(header) ) then
      write(lun0,'(A,A)') '========== timedate_t_dump: ',trim(header)
    else
      write(lun0,'(A,A)') '========== timedate_t_dump: ','Dump timedate_t'
    end if
    write(lun0,'(A10,": ",I0)') 'yr',self%yr
    write(lun0,'(A10,": ",I0)') 'mo',self%mo
    write(lun0,'(A10,": ",I0)') 'dy',self%dy
    write(lun0,'(A10,": ",I0)') 'hr',self%hr
    write(lun0,'(A10,": ",I0)') 'mn',self%mn
    write(lun0,'(A10,": ",I0)') 'sc',self%sc
    write(lun0,'(A10,": ",I0)') 'ms',self%ms
    write(lun0,'(A10,": ",SP, I0)') 'df',self%df
    write(lun0,'(A10,": ",I0)') 'days',self%dys
    write(lun0,'(A10,": ",I0)') 'secs',self%scs
    write(lun0,'(A10,": ",I0)') 'tsecs',self%tscs
    write(lun0,'(A10,": ",I0)') 'calen_type',self%cal%itype
    call self%cal%dump(lun0)
    write(lun0,'(A)') '========================================'
    
    return

  end subroutine timedate_t_dump
    
    
  !!========================================================================
  !> Set days then convert to yr-mo-dy.
  !!
  !! See the note in set_secs().
  !!
  subroutine timedate_t_set_days( self, days )
    class(timedate_t),intent(inout) :: self !< timedate_t instance
    integer(secs_k),intent(in) :: days !< days

    self%dys=days

    call self%cal%days2yymmdd( self%yr, self%mo, self%dy, days )

    self%tscs = days * self%cal%sc_dy + self%scs

    return

  end subroutine timedate_t_set_days
    


  !!========================================================================
  !> Set secs then convert to hr:mn:sc.
  !!
  !! - If `secs` > `sc_dy`, `dy` will be incremented,
  !! - else if `secs` is negative, `dy` will be decremented,
  !!
  !! so call me AFTER set_days()
  !!
  subroutine timedate_t_set_secs( self, secs )
    class(timedate_t),intent(inout) :: self !< timedate_t instance
    integer(secs_k),intent(in) :: secs !< secs

    integer(secs_k) :: s !! secs in day part
    integer(secs_k) :: d !! secs over sc_dy part


    s = mod(secs,self%cal%sc_dy)
    d = (secs-s)/self%cal%sc_dy

    if ( s < 0 ) then
      s = s + self%cal%sc_dy
      d = d - 1
    end if

    self%scs = s
    call self%cal%secs2hhmmss( self%hr, self%mn, self%sc, s )
    if ( d .ne. 0 ) then
      self%dys = self%dys + d
      call self%cal%days2yymmdd( self%yr, self%mo, self%dy, self%dys )
    end if

    self%tscs = self%dys * self%cal%sc_dy + self%scs

    return

  end subroutine timedate_t_set_secs
    

  !!========================================================================
  !> Set days and secs simultaneously.
  subroutine timedate_t_set_daysecs( self, days, secs )
    class(timedate_t),intent(inout) :: self !< timedate_t instance
    integer(secs_k),intent(in) :: days !< days
    integer(secs_k),intent(in) :: secs !< secs
    
    integer(secs_k) :: s 
    integer(secs_k) :: d 


    s = mod(secs,self%cal%sc_dy)
    d = (secs-s)/self%cal%sc_dy
    if ( s < 0 ) then
      s = s + self%cal%sc_dy
      d = d - 1
    end if

    self%scs = s
    self%dys = days + d
    self%tscs = self%dys * self%cal%sc_dy + self%scs

    call self%cal%secs2hhmmss( self%hr, self%mn, self%sc, self%scs )
    call self%cal%days2yymmdd( self%yr, self%mo, self%dy, self%dys )

    return
  end subroutine timedate_t_set_daysecs


  !!========================================================================
  !> Set total secs from epoch then convert to yr-mo-dyThr:mn:sc.
  !!
  !! \bug negative tsecs will cause invalid result.
  subroutine timedate_t_set_tsecs( self, tsecs )
    class(timedate_t),intent(inout) :: self !< timedate_t instance
    integer(secs_k),intent(in) :: tsecs     !< total secs

    integer(secs_k) :: s        !! secs
    integer(secs_k) :: d        !! days

    s = mod(tsecs, self%cal%sc_dy)
    d = (tsecs-s)/self%cal%sc_dy
    if ( s < 0 ) then
      s = s + self%cal%sc_dy
      d = d - 1
    end if

    self%scs = s
    self%dys = d
    self%tscs = tsecs

    !! At this point, scs is normalized, no bollow/carry happens.
    call self%cal%secs2hhmmss( self%hr, self%mn, self%sc, self%scs )
    call self%cal%days2yymmdd( self%yr, self%mo, self%dy, self%dys )

    return

  end subroutine timedate_t_set_tsecs



  !!========================================================================
  !> Set members from values.
  !!
  !! `days` and `secs` are updated and vals are normalized on return.
  !!
  !! \todo normalize hr:mn:sc
  !!
  subroutine timedate_t_set_from_values(self,&
    & yr, mo, dy, hr, mn, sc, ms, df)
    class(timedate_t),intent(inout) :: self !< timedate_t instance
    integer(secs_k),intent(in),optional :: yr !< year                           
    integer(secs_k),intent(in),optional :: mo !< month                          
    integer(secs_k),intent(in),optional :: dy !< day                            
    integer(secs_k),intent(in),optional :: hr !< hour                           
    integer(secs_k),intent(in),optional :: mn !< minute                         
    integer(secs_k),intent(in),optional :: sc !< second                         
    integer(secs_k),intent(in),optional :: ms !< milli-second                   
    integer(secs_k),intent(in),optional :: df !< difference with UTC in minutes
    
    integer(secs_k) :: ddd
    integer(secs_k) :: ms0
    integer(secs_k) :: sc1

    integer(secs_k) :: secs
    integer(secs_k) :: dc
    integer(secs_k) :: ss

    if ( present(yr) ) self%yr = yr
    if ( present(mo) ) self%mo = mo
    if ( present(dy) ) self%dy = dy
    if ( present(hr) ) self%hr = hr
    if ( present(mn) ) self%mn = mn
    if ( present(sc) ) self%sc = sc
    if ( present(ms) ) self%ms = ms
    if ( present(df) ) self%df = df

    !! milli-sec and sec
    if ( self%ms .ne. 0 ) then
      ms0 = mod(ms, 1000)
      sc1 = ms/1000
      if ( ms0 < 0 ) then
        ms0 = ms0 + 1000
        sc1 = sc1 -1
      end if
      self%ms = ms0
      self%sc = self%sc + sc1
    end if

    
    !! secs and day
    call self%cal%hhmmss2secs(secs, self%hr, self%mn, self%sc)
    dc = secs/self%cal%sc_dy
    ss = mod(secs,self%cal%sc_dy)
    if ( ss < 0 ) then
      ss = ss + self%cal%sc_dy
      dc = dc - 1
    end if

    self%scs = ss
    self%dy = self%dy + dc

    call self%cal%secs2hhmmss(self%hr, self%mn, self%sc, self%scs)

    !! below are the same with the latter half of set_from_values()
    call self%cal%yymmdd2yyddd(ddd, self%yr, self%mo, self%dy )
    call self%cal%yyddd2days(self%dys,self%yr,ddd)
    call self%cal%yyddd2yymmdd(self%yr,self%mo,self%dy,ddd)


    self%tscs = self%dys * self%cal%sc_dy + self%scs

    return

  end subroutine timedate_t_set_from_values



  !!========================================================================
  !> Set members from iso or cdate format strings.
  !!
  !! \note
  !! When `ciso` is given, cdate/ctime/czone are neglected even if present.
  !! 
  subroutine timedate_t_set_from_strings( self, cdate, ctime, czone, ciso )
    class(timedate_t),intent(inout) :: self !< timedate_t instance
    character(len=*), intent(in),optional :: cdate !< cdate format, len=8 or 10
    character(len=*), intent(in),optional :: ctime !< ctime format, len=6, 8, 10 or 12
    character(len=*), intent(in),optional :: czone !< czone format, len=5 or 'Z'
    character(len=*), intent(in),optional :: ciso  !< iso8601 format, len=20 or 25

    integer(secs_k) :: vals(8)

    if ( present( ciso ) ) then
      call iso2vals( vals, ciso )
    else 
!!$      write(*,*)'dbg:set_from_strings:','hoge'
      if ( present(cdate) )  call cdate2vals( vals, cdate )
      if ( present(ctime) )  call ctime2vals( vals, ctime )
      if ( present(czone) )  call czone2vals( vals, czone )
    end if

!!$    write(*,*)'dbg:set_from_strings:vals',vals
    
    call self%set_from_values(&
      &  yr=vals(1), mo=vals(2), dy=vals(3), &
      &  hr=vals(5), mn=vals(6), sc=vals(7), &
      &  ms=vals(8), df=vals(4) )

!!$    call self%cal%yymmdd2days( self%dys, self%yr, self%mo, self%dy )
!!$    call self%cal%hhmmss2secs( self%scs, self%hr, self%mn, self%sc )
!!$
!!$    self%tscs = self%dys * self%cal%sc_dy + self%scs
    return

  end subroutine timedate_t_set_from_strings



  !!========================================================================
  !> Set value with unit
  !!
  !! The first 2 letters of `unit` are meaningful.
  !! 
  subroutine timedate_t_set_w_unit( self, val, unit )
    class(timedate_t),intent(inout) :: self !< timedate_t instance
    integer(secs_k),intent(in) :: val     !< value
    character(len=2),intent(in) :: unit      !< unit.

    select case ( unit )
    case ('YR', 'Yr', 'yr' )
      call self%set_from_values( yr=val )
    case ('MO', 'Mo', 'mo' )
      call self%set_from_values( mo=val )
    case ('DY', 'Dy', 'dy' )
      call self%set_from_values( dy=val )
    case ('HR', 'Hr', 'hr' )
      call self%set_from_values( hr=val )
    case ('MN', 'Mn', 'mn' )
      call self%set_from_values( mn=val )
    case ('SC', 'Sc', 'sc' )
      call self%set_from_values( sc=val )
    case default
      write(0,*)'timedate_t_set_w_unit: Invalid unit: '//trim(unit)
      call exit(1)
    end select

    return

  end subroutine timedate_t_set_w_unit



  !!========================================================================
  !! return values
  !!========================================================================

  !!========================================================================
  !> Return values(8) array
  !!
  !! Order of values are the same with date_and_time()
  function timedate_t_get_values(self) result (res)
    class(timedate_t),intent(in) :: self !< timedate_t instance
    integer(secs_k) :: res(8)            !< date_and_time() values

    res = [self%yr, self%mo, self%dy, self%df, self%hr, self%mn, self%sc, self%ms]

    return
  end function timedate_t_get_values


  !!========================================================================
  !> Return days
  function timedate_t_get_days(self) result (res)
    class(timedate_t),intent(in) :: self !< timedate_t instance
    integer(secs_k) :: res               !< days

    res = self%dys

    return
  end function timedate_t_get_days



  !!========================================================================
  !> Return secs
  function timedate_t_get_secs(self) result (res)
    class(timedate_t),intent(in) :: self !< timedate_t instance
    integer(secs_k) :: res               !< secs

    res = self%scs

    return
  end function timedate_t_get_secs


  !!========================================================================
  !> Return total secs
  function timedate_t_get_tsecs(self) result (res)
    class(timedate_t),intent(in) :: self !< timedate_t instance
    integer(secs_k) :: res               !< total secs of given self.

    res = self%tscs

    return
  end function timedate_t_get_tsecs


  !!========================================================================
  !> Return days per year of self
  function timedate_t_get_dy_yr(self) result (res)
    class(timedate_t),intent(in) :: self !< timedate_t instance
    integer(secs_k) :: res               !< days per year of given self.

    res = self%cal%get_dy_yr(self%yr)

  end function timedate_t_get_dy_yr


  !!========================================================================
  !> Return months per year of self
  function timedate_t_get_mo_yr(self) result (res)
    class(timedate_t),intent(in) :: self !< timedate_t instance
    integer(secs_k) :: res               !< months per year of given self.

    res = self%cal%mo_yr

  end function timedate_t_get_mo_yr


  !!========================================================================
  !> Return seconds per day of self
  function timedate_t_get_sc_dy(self) result (res)
    class(timedate_t),intent(in) :: self !< timedate_t instance
    integer(secs_k) :: res               !< seconds per day of given self.

    res = self%cal%get_sc_dy()

  end function timedate_t_get_sc_dy
    



  !!========================================================================
  !> query specified value
  !!
  !! 
  subroutine timedate_t_query_values( self, yr, mo, dy, hr, mn, sc, ms )
    class(timedate_t),intent(in) :: self !< timedate_t instance
    integer(secs_k),intent(out),optional :: yr !< year
    integer(secs_k),intent(out),optional :: mo !< year
    integer(secs_k),intent(out),optional :: dy !< year
    integer(secs_k),intent(out),optional :: hr !< year
    integer(secs_k),intent(out),optional :: mn !< year
    integer(secs_k),intent(out),optional :: sc !< year
    integer(secs_k),intent(out),optional :: ms !< year

    if ( present( yr ) ) yr = self%yr
    if ( present( mo ) ) mo = self%mo
    if ( present( dy ) ) dy = self%dy
    if ( present( hr ) ) hr = self%hr
    if ( present( mn ) ) mn = self%mn
    if ( present( sc ) ) sc = self%sc
    if ( present( ms ) ) ms = self%ms

    return

  end subroutine timedate_t_query_values





  !!========================================================================
  !! return strings in various format
  !!========================================================================


  !!========================================================================
  !> Return iso 8601 basic format strings
  function timedate_t_format_iso_bas(self) result (res)
    class(timedate_t),intent(in) :: self !< timedate_t instance
    character(len=20) :: res             !< iso 8601 basic format strings

    !!   12345678901234567890
    res='YYYYMMDDThhmmss+MMSS'

    write(res(1:15),form_iso_bas)self%yr,self%mo,self%dy,self%hr,self%mn,self%sc
    if ( self%df == 0 ) then
      write(res(16:20),'(A)')  'Z'
    else
      write(res(16:20),form_czone) self%df/60, mod(self%df,60)
    end if
    return

  end function timedate_t_format_iso_bas



  !!========================================================================
  !> Return iso 8601 extended format strings
  function timedate_t_format_iso_ext(self) result (res)
    class(timedate_t),intent(in) :: self !< timedate_t instance
    character(len=25) :: res             !< iso 8601 extended format strings

    !!   1234567890123456789012345
    res='YYYY-MM-DDThh:mm:ss+09:00'

    write(res(1:19),form_iso_ext)self%yr,self%mo,self%dy,self%hr,self%mn,self%sc
    if ( self%df == 0 ) then
      write(res(20:25),'(A)') 'Z'
    else
      write(res(20:25),form_czone_ext) self%df/60, mod(self%df,60)
    end if

    return

  end function timedate_t_format_iso_ext



  !!========================================================================
  !> Return date part of date_and_time() format strings
  function timedate_t_format_cdate(self) result (res)
    class(timedate_t),intent(in) :: self !< timedate_t instance
    character(len=8) :: res              !< date part of date_and_time() format strings

    write(res,form_cdate) self%yr, self%mo, self%dy

    return

  end function timedate_t_format_cdate


  !!========================================================================
  !> Return time part of date_and_time format strings
  function timedate_t_format_ctime(self) result (res)
    class(timedate_t),intent(in) :: self !< timedate_t instance
    character(len=10) :: res             !< time part of date_and_time() format strings

    write(res,form_ctime) self%hr, self%mn, self%sc, self%ms

    return

  end function timedate_t_format_ctime


  !!========================================================================
  !> Return tzone part of date_and_time format strings
  function timedate_t_format_czone(self) result (res)
    class(timedate_t),intent(in) :: self !< timedate_t instance
    character(len=5) :: res              !< tzone part of date_and_time() format strings

    if ( self%df == 0 ) then
      res = 'Z'
    else
      write(res,form_czone) self%df/60, mod(self%df,60)
    end if

    return

  end function timedate_t_format_czone


  !!========================================================================
  !> Shift timezone
  !!
  !! `df` denotes target timezone by difference in minutes from UTC
  !!
  subroutine timedate_t_shift_tz(self,df)
    class(timedate_t),intent(inout) :: self !< timedate_t instance
    integer(secs_k),intent(in) :: df !< difference in minutes

    integer(secs_k) :: dmn

    if ( df .eq. self%df ) return !! no need to shift

    dmn = df-self%df

    call self%set_from_values( mn=dmn, df=df )

    return

  end subroutine timedate_t_shift_tz



  !!========================================================================
  !> Add days.
  !!
  subroutine timedate_t_add_days( self, days )
    class(timedate_t),intent(inout) :: self !< timedate_t instance
    integer(secs_k),intent(in) :: days      !< days to be add/sub

    integer(secs_k) :: d0

    d0 = self%dys

    call self%set_days( d0 + days )

    return

  end subroutine timedate_t_add_days


  !!========================================================================
  !> Add secs.
  !!
  subroutine timedate_t_add_secs( self, secs )
    class(timedate_t),intent(inout) :: self !< timedate_t instance
    integer(secs_k),intent(in) :: secs      !< secs to be add/sub

    integer(secs_k) :: s0

    s0 = self%scs

    call self%set_secs( s0 + secs )

    return

  end subroutine timedate_t_add_secs



  !!========================================================================
  !> Add total secs.
  !!
  subroutine timedate_t_add_tsecs( self, tsecs )
    class(timedate_t),intent(inout) :: self !< timedate_t instance
    integer(secs_k),intent(in) :: tsecs     !< total secs to be add/sub

    integer(secs_k) :: s0

    s0 = self%tscs

    call self%set_tsecs( s0 + tsecs )

    return

  end subroutine timedate_t_add_tsecs



  !!========================================================================
  !! Add values except df.
  !!
  subroutine timedate_t_add_values( self, yr, mo, dy, hr, mn, sc, ms )
    class(timedate_t),intent(inout) :: self !< timedate_t instance
    integer(secs_k),intent(in),optional :: yr      !< years
    integer(secs_k),intent(in),optional :: mo      !< months
    integer(secs_k),intent(in),optional :: dy      !< days
    integer(secs_k),intent(in),optional :: hr      !< hours
    integer(secs_k),intent(in),optional :: mn      !< minutes
    integer(secs_k),intent(in),optional :: sc      !< seconds
    integer(secs_k),intent(in),optional :: ms      !< milli-seconds

    integer(secs_k) :: ddd
    integer(secs_k) :: ms0
    integer(secs_k) :: sc1

    integer(secs_k) :: secs
    integer(secs_k) :: dc
    integer(secs_k) :: ss


    if ( present(yr) ) self%yr = self%yr + yr
    if ( present(mo) ) self%mo = self%mo + mo
    if ( present(dy) ) self%dy = self%dy + dy
    if ( present(hr) ) self%hr = self%hr + hr
    if ( present(mn) ) self%mn = self%mn + mn
    if ( present(sc) ) self%sc = self%sc + sc
    if ( present(ms) ) self%ms = self%ms + ms

    !! milli-sec and sec
    if ( self%ms .ne. 0 ) then
      ms0 = mod(ms, 1000)
      sc1 = ms/1000
      if ( ms0 < 0 ) then
        ms0 = ms0 + 1000
        sc1 = sc1 -1
      end if
      self%ms = ms0
      self%sc = self%sc + sc1
    end if

    
    !! secs and day
    call self%cal%hhmmss2secs(secs, self%hr, self%mn, self%sc)
    dc = secs/self%cal%sc_dy
    ss = mod(secs,self%cal%sc_dy)
    if ( ss < 0 ) then
      ss = ss + self%cal%sc_dy
      dc = dc - 1
    end if

    self%scs = ss
    self%dy = self%dy + dc

    call self%cal%secs2hhmmss(self%hr, self%mn, self%sc, self%scs)

    !! below are the same with the latter half of set_from_values()
    call self%cal%yymmdd2yyddd(ddd, self%yr, self%mo, self%dy )
    call self%cal%yyddd2days(self%dys,self%yr,ddd)
    call self%cal%yyddd2yymmdd(self%yr,self%mo,self%dy,ddd)


    self%tscs = self%dys * self%cal%sc_dy + self%scs

    return

  end subroutine timedate_t_add_values
  


  !!========================================================================
  !! Add value with unit.
  !!
  !! Valid `unit` are 'YR', 'MO', 'DY', 'HR', 'MN', 'SC'.
  !! 
  subroutine timedate_t_add_w_unit( self, val, unit )
    class(timedate_t),intent(inout) :: self !< timedate_t instance
    integer(secs_k),intent(in) :: val     !< value
    character(len=*),intent(in) :: unit      !< unit.

!!$    write(*,*)'dbg:timedate_t_add_w_unit:val,unit:',val,unit
    select case ( unit(1:2) )
    case ( 'YE', 'Ye', 'ye', 'YR', 'Yr', 'yr' )
      call self%add_values( yr=val )
    case ('MO', 'Mo', 'mo' )
      call self%add_values( mo=val )
    case ( 'DA', 'Da', 'da', 'DY', 'Dy', 'dy' )
      call self%add_values( dy=val )
    case ( 'HO', 'Ho', 'ho', 'HR', 'Hr', 'hr' )
      call self%add_values( hr=val )
    case ( 'MI', 'Mi', 'mi', 'MN', 'Mn', 'mn' )
      call self%add_values( mn=val )
    case ( 'SE', 'Se', 'se', 'SC', 'Sc', 'sc' )
      call self%add_values( sc=val )
    case default
      write(0,*)'timedate_t_add_w_unit: Invalid unit: '//trim(unit)
      call exit(1)
    end select

    return

  end subroutine timedate_t_add_w_unit


  !!========================================================================
  !> Assignment: lefthand side will be newly allocated.
  !!
  subroutine timedate_t_alloc_assgn(r, l)
    class(timedate_t),intent(in) :: r !< righthand side
    class(timedate_t),intent(out),allocatable :: l !< lefthand side

    allocate(l,source=r)

    return

  end subroutine timedate_t_alloc_assgn


  !!========================================================================
  !> Assignment: lefthand side is old and copy members each by each.
  !!
  !! For the case l is already allocated or not allocatable.
  !!
  !! \bug Is this useless ??
!!$  subroutine timedate_t_copy(r, l)
!!$    class(timedate_t),intent(in)  :: r !< righthand side
!!$    class(timedate_t),intent(out) :: l !< lefthand side
!!$
!!$    l%yr   = r%yr  
!!$    l%mo   = r%mo  
!!$    l%dy   = r%dy  
!!$    l%hr   = r%hr  
!!$    l%mn   = r%mn  
!!$    l%sc   = r%sc  
!!$    l%ms   = r%ms  
!!$    l%df   = r%df  
!!$    l%dys  = r%dys 
!!$    l%scs  = r%scs 
!!$    l%tscs = r%tscs
!!$
!!$    return
!!$
!!$  end subroutine timedate_t_copy
    


  !!========================================================================
  !> Duration of `other` from `self`, return [days,secs]
  !!
  function timedate_t_sub(self, other) result (res)
    class(timedate_t),intent(in) :: self
    class(timedate_t),intent(in) :: other
    integer(secs_k) :: res(2) !< [days,scs]

    integer(secs_k) :: dd
    integer(secs_k) :: ss

!!$    write(*,*)'dbg:timedate_t_sub:days:',other%get_days(),self%get_days()
!!$    write(*,*)'dbg:timedate_t_sub:secs:',other%get_secs(),self%get_secs()

    dd = other%get_days() - self%get_days()
    ss = other%get_secs() - self%get_secs()

    if ( ss < 0 ) then
      dd = dd - 1 
      ss = ss + self%cal%sc_dy
    end if

    res = [dd,ss]

    return

  end function timedate_t_sub


  !!========================================================================
  !! Subtract t2-t1, return total secs
  





  !!========================================================================
  !> Set default calendar type.
  !!
  !! \note This is NOT a member method of timedate_t class.
  subroutine timedate_t_set_default_calen( itype )
    integer,intent(in) :: itype

    calen_default = itype

    return

  end subroutine timedate_t_set_default_calen



!!$========================================================================
!!$ Private routines
!!$========================================================================


  !!========================================================================
  !! ciso to values
  !!
  !! \note
  !! On return values are NOT NORMALIZED.
  !!
  subroutine iso2vals( vals, ciso )
    integer(secs_k),intent(out) :: vals(8) !< 8 integer values same with date_and_time().
    character(len=*), intent(in),optional :: ciso  !< iso8601 format, len=20 or 25
    
    integer(secs_k) :: dfh !! hour part of df
    integer(secs_k) :: dfm !! minutes part of df
    logical :: is_iso_ext

    integer(secs_k) :: ios
    character(len=80) :: emsg


    if (      (len_trim(ciso) == 20 .or. len_trim(ciso) == 25 )&
      & .and. index(ciso,'-') > 0 .and. index(ciso,':') > 0    ) then
      is_iso_ext = .true.
    else if   (len_trim(ciso) == 16 .or. len_trim(ciso) == 20 ) then
      is_iso_ext = .false.
    else
      write(0,*)'set_from_strings: Invalid iso8601 format: '//trim(ciso)
      call exit(1)
    end if

!!$    write(*,*)'dbg:set_from_strings:ciso,is_iso_ext: ',ciso,is_iso_ext

    if ( is_iso_ext ) then
      read(ciso(1:19),form_iso_ext0,iostat=ios,iomsg=emsg)&
        & vals(1), vals(2), vals(3), vals(5), vals(6), vals(7)
      if ( ios .ne. 0 ) then
        write(0,*)'set_from_strings: Error in reading iso8601 format: '//trim(emsg)
        call exit(1)
      end if
      if ( ciso(20:20) .eq. 'Z' ) then
        vals(4) = 0
      else
        read(ciso(20:25),form_czone_ext0,iostat=ios,iomsg=emsg) dfh, dfm
        vals(4) = dfh * 60 + dfm
        if ( ios .ne. 0 ) then
          write(0,*)'set_from_strings: Error in reading iso8601 format: '//trim(emsg)
          call exit(1)
        end if
      end if
    else
      read(ciso(1:15),form_iso_bas0,iostat=ios,iomsg=emsg)&
        & vals(1), vals(2), vals(3), vals(5), vals(6), vals(7)
      if ( ios .ne. 0 ) then
        write(0,*)'set_from_strings: Error in reading iso8601 format: '//trim(emsg)
        call exit(1)
      end if
      if ( ciso(16:16) .eq. 'Z' ) then
        vals(4) = 0
      else
        read(ciso(16:20),form_czone,iostat=ios,iomsg=emsg) dfh, dfm
        vals(4) = dfh * 60 + dfm
        if ( ios .ne. 0 ) then
          write(0,*)'set_from_strings: Error in reading iso8601 format: '//trim(emsg)
          call exit(1)
        end if
      end if
    end if
    return !! cdate/ctime/czone are neglected.
  end subroutine iso2vals
  


  !!========================================================================
  !! cdate to values
  subroutine cdate2vals( vals, cdate )
    integer(secs_k),intent(out) :: vals(8) !< 8 integer values same with date_and_time().
    character(len=*),intent(in) :: cdate  !< cdate format, len=8 or 10

    integer :: ios
    character(len=80) :: emsg

!!$    write(*,*)'dbg:set_from_strings:','cdate:',trim(cdate)

    select case ( len_trim(cdate) )
    case (  8 ) !! "YYYYMMDD"
      read(cdate,form_cdate1,iostat=ios,iomsg=emsg ) vals(1),vals(2),vals(3)
    case ( 10 ) !! "YYYY-MM-DD"
      read(cdate,form_cdate2,iostat=ios,iomsg=emsg ) vals(1),vals(2),vals(3)
    case default
      write(0,*)'set_from_strings: invalid cdate format:'//trim(cdate)
      call exit(1)
    end select
    if ( ios .ne. 0 ) then
      write(0,*)'set_from_strings: Error in reading cdate: '//trim(emsg)
      call exit(1)
    end if

    return

  end subroutine cdate2vals


  !!========================================================================
  !! ctime to values
  subroutine ctime2vals( vals, ctime )
    integer(secs_k),intent(out) :: vals(8) !< 8 integer values same with date_and_time().
    character(len=*), intent(in)    :: ctime !< ctime format, len=6, 8, 10 or 12

    integer :: ios
    character(len=80) :: emsg

!!$    write(*,*)'dbg:set_from_strings:','ctime:',trim(ctime)

    select case ( len_trim(ctime) )
    case (  6 ) !! "hhmmss"
      read(ctime,form_ctime1,iostat=ios,iomsg=emsg ) vals(5), vals(6), vals(7)
    case (  8 ) !! "hh:mm:ss"
      read(ctime,form_ctime2,iostat=ios,iomsg=emsg ) vals(5), vals(6), vals(7)
    case ( 10 ) !! "hhmmss.sss"
      read(ctime,form_ctime3,iostat=ios,iomsg=emsg ) vals(5), vals(6), vals(7), vals(8)
    case ( 12 ) !! "hh:mm:ss.sss"
      read(ctime,form_ctime4,iostat=ios,iomsg=emsg ) vals(5), vals(6), vals(7), vals(8)
    case default
      write(0,*)'set_from_strings: invalid ctime format:'//trim(ctime)
      call exit(1)
    end select
    if ( ios .ne. 0 ) then
      write(0,*)'set_from_strings: Error in reading ctime: '//trim(emsg)
      call exit(1)
    end if
    return
  end subroutine ctime2vals


  !!========================================================================
  !! czone to values
  subroutine czone2vals( vals, czone )
    integer(secs_k),intent(out) :: vals(8) !< 8 integer values same with date_and_time().
    character(len=*), intent(in)    :: czone !< czone format, len=5 or 'Z'
    
    integer :: dfh !! hour part of df
    integer :: dfm !! minutes part of df

    integer :: ios
    character(len=80) :: emsg

!!$    write(*,*)'dbg:set_from_strings:','czone:',trim(czone)

    if ( czone=='Z' ) then
      vals(4) = 0
    else if ( len(czone) == 5 ) then
      read(czone,form_czone,iostat=ios,iomsg=emsg ) dfh, dfm
      vals(4) = dfh*60 + dfm
    else
      write(0,*)'set_from_strings: invalid czone format:'//trim(czone)
      call exit(1)
    end if

    if ( ios .ne. 0 ) then
      write(0,*)'set_from_strings: Error in reading czone: '//trim(emsg)
      call exit(1)
    end if

    return
  end subroutine czone2vals

end module mod_tdc_timedate_type



!!$************************************************************************
#ifdef TEST
!!$************************************************************************
program test_tdc_timedate_type
  use mod_tdc_timedate_type
  implicit none 

  integer :: itest
  character(len=2) :: ctest
  integer :: itype
  character(len=2) :: ctype

  type(timedate_t) :: t, t1, t2
  integer(secs_k) :: vals(8)
  character(len=8) :: cdate
  character(len=10) :: ctime
  character(len=5) :: czone
  character(len=20) :: ciso_bas
  character(len=25) :: ciso_ext

  integer,parameter :: num = 20
  integer(secs_k) :: ds(2,num) !! [days,secs]
  integer(secs_k) :: dd(num) !! [days]
  integer(secs_k) :: ss(num) !! [secs]
  integer(secs_k) :: yr,mo,dy
  integer(secs_k) :: hr,mn,sc


  integer :: i
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

  cdate='20150520'
  ctime='120546.982'
  czone='+0900'
  vals=[2015,   5,  20, 540,  12,   5,  46, 982]
  write(ciso_bas,form_iso_bas)vals(1),vals(2),vals(3),vals(5),vals(6),vals(7)
  write(ciso_ext,form_iso_ext)vals(1),vals(2),vals(3),vals(5),vals(6),vals(7)
  write(ciso_bas(16:20),form_czone)vals(4)/60,mod(vals(4),60)
  write(ciso_ext(20:25),form_czone_ext)vals(4)/60,mod(vals(4),60)

  select case ( itype )
  case ( 1 )
    call timedate_t_set_default_calen( calen_ProGre )
  case ( 2 )
    call timedate_t_set_default_calen( calen_ProGre_NL )
  case ( 3 ) 
    call timedate_t_set_default_calen( calen_ProGre )
    vals(4)=0;    ciso_bas(16:16)='Z';    ciso_ext(20:20)='Z'
  case ( 4 ) 
    call timedate_t_set_default_calen( calen_ProGre_NL )
    vals(4)=0;    ciso_bas(16:16)='Z';    ciso_ext(20:20)='Z'
  case default
    !! use default
  end select

  select case ( itest )
  case ( 1 )
    write(*,'(A,8(I4,:,","))')  "***** set from vals: ",vals
    call t%init()
    call t%set_from_values(&
      & yr=vals(1), mo=vals(2), dy=vals(3), &
      & hr=vals(5), mn=vals(6), sc=vals(7), &
      & ms=vals(8), df=vals(4) )
    call t%dump('set_from_values')
    call result()

    write(*,*)
    write(*,'(A,3(A,:,","))')  "***** set from cdates: ",cdate, ctime, czone
    call t%init()
    call t%set_from_strings( cdate, ctime, czone )
    call t%dump('set_from_strings')
    call result()

    write(*,*)("+",i=1,80)
    write(*,'(A,A)')               "***** set from ciso_bas: ",ciso_bas
    call t%init()
    call t%set_from_strings( ciso=ciso_bas )
    call t%dump('set_from_strings(iso_bas)')
    call result()

    write(*,*)
    write(*,'(A,A)')               "***** set from ciso_ext: ",ciso_ext
    call t%init()
    call t%set_from_strings( ciso=ciso_ext )
    call t%dump('set_from_strings(iso_ext)')
    call result()
  case ( 5 )
    vals=[2015,-5,12,0,12,0,0,0]
    write(*,'(A,8(I4,:,","))')  "***** set from vals with un-normalized: ",vals
    call t%init()
    call t%set_from_values(vals(1),vals(2),vals(3),vals(5),vals(6),vals(7))
!!$    call t%dump('set_from_vals(w/o normalize)')
    call result()

    vals=[2015,5,-12,0,12,0,0,0]
    write(*,'(A,8(I4,:,","))')  "***** set from vals with un-normalized: ",vals
    call t%init()
    call t%set_from_values(vals(1),vals(2),vals(3),vals(5),vals(6),vals(7))
!!$    call t%dump('set_from_vals(w/o normalize)')
    call result()

    vals=[2015,5,-420,0,12,0,0,0]
    write(*,'(A,8(I4,:,","))')  "***** set from vals with un-normalized: ",vals
    call t%init()
    call t%set_from_values(vals(1),vals(2),vals(3),vals(5),vals(6),vals(7))
!!$    call t%dump('set_from_vals(w/o normalize)')
    call result()

  case ( 21 )
    write(*,'(A)')  "***** test of set_daysecs()"
    n = 3
    ds(:,1:n) = reshape(&
      &         [735730,59853,         &  ! is 2015-05-12T16:37:33 in calen_ProGre
      &          735730,59853+86400  , &  ! is 2015-05-12T16:37:33 + 1dy
      &          735730,59853-86400  ],&  ! is 2015-05-12T16:37:33 - 1dy
      & [2,n])

    do i=1,n
      call t%init()
      call t%set_daysecs(ds(1,i),ds(2,i))
      write(*,'(A,2I8)') '***** Set [days,secs]=',ds(:,i)
      call result()
      write(*,*)
    end do

  case ( 22 )
    write(*,'(A)')  "***** test of add_days()"
    ds(:,1) = [735730,59853] ! is 2015-05-12T16:37:33 in calen_ProGre
    n = 6
    dd(1:n) = [30,-30, 370, -370, 1260, -1260] ! days
    do i = 1, n
      call t%init()
      call t%set_daysecs(ds(1,1),ds(2,1))
      write(*,'(A,A)') '***** Current: ', t%format_iso_ext()
      call t%add_days(dd(i))
      write(*,'(A,I0,A,A)') '***** Add ',dd(i),'dys: ', t%format_iso_ext()
    end do
    write(*,*)

  case ( 23 )
    write(*,'(A)')  "***** test of add_secs()"
    ds(:,1) = [735730,59853] ! is 2015-05-12T16:37:33 in calen_ProGre
    n = 8
    ss(1:n) = [30,-30, 370, -370, 3700, -3700, 370000,-370000] ! secs
    do i = 1, n
      call t%init()
      call t%set_daysecs(ds(1,1),ds(2,1))
      write(*,'(A,A)') '***** Current: ', t%format_iso_ext()
      call t%add_secs(ss(i))
      write(*,'(A,I0,A,A)') '***** Add ',ss(i),'scs: ', t%format_iso_ext()
    end do
    write(*,*)

  case ( 24 )
    write(*,'(A)')  "***** test of set_tsecs()"
    n = 1
    ss(1:n) = [63567806746_secs_k] ! is 2015-05-20T12:05:46 in calen_ProGre

    do i = 1, n
      call t%init()
      call t%set_tsecs(ss(i))
      write(*,'(A,A)') '***** Current: ', t%format_iso_ext()
    end do
    write(*,*)

  case ( 25 ) 
    write(*,'(A)')  "***** test of add_values()"
    call t%init()
    call t%set_from_strings( ciso=ciso_bas )
    write(*,'(A,A)') '***** Current: ', t%format_iso_ext()
    call t%add_values( mo=12_secs_k); write(*,'(A,8I5)') '***** result: ', t%get_values()
    call t%add_values( dy=-235_secs_k);    write(*,'(A,8I5)') '***** result: ', t%get_values()
    call t%add_values( hr=75_secs_k);    write(*,'(A,8I5)') '***** result: ', t%get_values()
    call t%add_values( mn=-760_secs_k);    write(*,'(A,8I5)') '***** result: ', t%get_values()
    call t%add_values( sc=320_secs_k);    write(*,'(A,8I5)') '***** result: ', t%get_values()
    call t%add_values( ms=-888_secs_k);    write(*,'(A,8I5)') '***** result: ', t%get_values()

  case ( 31 )
    Dy = 735730
    call t1%init();    call t1%set_days(dy)
    call t2%init();    call t2%set_days(dy+10)
    ds(:,1) = t1%sub(t2) ! t2-t1
    write(*,*)'t1:',t1%format_iso_bas()
    write(*,*)'t2:',t2%format_iso_bas()
!!$    call t1%dump('t1')
!!$    call t2%dump('t2')
    write(*,*)'sub t2-t1:',ds(:,1)

    write(*,*)

    dy = 59853
    call t1%init();    call t1%set_secs(dy)
    call t2%init();    call t2%set_secs(dy+100)
    ds(:,1) = t1%sub(t2) ! t2-t1
    write(*,*)'t1:',t1%format_iso_bas()
    write(*,*)'t2:',t2%format_iso_bas()
    write(*,*)'sub t2-t1:',ds(:,1)

    write(*,*)

    
    call t1%init();
    yr = 2015; mo = 5; dy = 12
    call t1%set_from_values(yr,mo,dy)
    call t2%init();
    yr = 2015; mo = 6; dy = 1
    call t2%set_from_values(yr,mo,dy)
    ds(:,1) = t1%sub(t2) ! t2-t1
    write(*,*)'t1:',t1%format_iso_bas()
    write(*,*)'t2:',t2%format_iso_bas()
    write(*,*)'sub t2-t1:',ds(:,1)

    write(*,*)

    call t1%init();
    yr=2015; mo=5; dy=12; hr=12; mn=0; sc=0
    call t1%set_from_values(yr,mo,dy,hr,mn,sc)
    call t2%init();
    yr=2015; mo=6; dy=1; hr=0; mn=0; sc=0
    call t1%set_from_values(yr,mo,dy,hr,mn,sc)
    ds(:,1) = t1%sub(t2) ! t2-t1
    write(*,*)'t1:',t1%format_iso_bas()
    write(*,*)'t2:',t2%format_iso_bas()
    write(*,*)'sub t2-t1:',ds(:,1)

  case ( 41 )
    write(*,'(A)')  "***** test of shift_tz()"
    call t%init()
    call t%set_from_strings( ciso=ciso_bas )
    write(*,'(A,A)') '***** Current: ', t%format_iso_ext()
    call t%shift_tz( +0_secs_k ) !! +09:00 to UTC
    write(*,'(A,A)') '***** UTC: ', t%format_iso_ext()

  case ( 51 )
    write(*,'(A)')  "***** test of get_dy_yr()"
    call t%init()
    yr = 1996; mo = 6; dy = 1;    call t%set_from_values(yr,mo,dy)
    write(*,'(A,A)') '***** Current: ', t%format_iso_ext()
    write(*,'(A,I0)') '***** dy_yr: ', t%get_dy_yr()
    yr = 2000; mo = 6; dy = 1;    call t%set_from_values(yr,mo,dy)
    write(*,'(A,A)') '***** Current: ', t%format_iso_ext()
    write(*,'(A,I0)') '***** dy_yr: ', t%get_dy_yr()
    yr = 2015; mo = 6; dy = 1;    call t%set_from_values(yr,mo,dy)
    write(*,'(A,A)') '***** Current: ', t%format_iso_ext()
    write(*,'(A,I0)') '***** dy_yr: ', t%get_dy_yr()
    yr = 2020; mo = 6; dy = 1;    call t%set_from_values(yr,mo,dy)
    write(*,'(A,A)') '***** Current: ', t%format_iso_ext()
    write(*,'(A,I0)') '***** dy_yr: ', t%get_dy_yr()
    


  case default
    call exit(1)
  end select


  call exit(0)


contains
  subroutine prepare()

    
!!$    write(*,'(A)') '****** Preparation: ******'
!!$    write(*,'(A12,8I4)')             "vals: ",vals
!!$    write(*,'(A12,A,1X,A,1X, A)')    "cdate's: ",cdate, ctime, czone
!!$    write(*,'(A12,A)')               "ciso_bas: ",ciso_bas
!!$    write(*,'(A12,A)')               "ciso_ext: ",ciso_ext

    return
  end subroutine prepare




  subroutine result()
    write(*,'(A)') '***** Results:'
    write(*,'(A18,8I6)') 'get_values: ',t%get_values()
    write(*,'(A18,8I6)') 'get_days: ',t%get_days()
    write(*,'(A18,8I6)') 'get_secs: ',t%get_secs()
    write(*,'(A18,A)') 'format_iso_bas: ',t%format_iso_bas()
    write(*,'(A18,A)') 'format_iso_ext: ',t%format_iso_ext()
    write(*,'(A18,A)') 'format_cdate: ',t%format_cdate()
    write(*,'(A18,A)') 'format_ctime: ',t%format_ctime()
    write(*,'(A18,A)') 'format_czone: ',t%format_czone()

    return

  end subroutine result
  
end program test_tdc_timedate_type
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
  
