#define TIME_STAMP 'Time-stamp: <2015/08/24 14:00:23 JST by Takahiro INOUE mod_tdc_calen_type.F03>'
!> Module to define calen_t class, sub module of mod_tdc.
!!
!! This class manages calendar related parameters and
!! provides some methods for calculation of date.
!!
!! \par notation
!! - yy : year in 4digit
!! - mm : month in the year
!! - dd : day in the month
!! - ddd : days in the year
!! .
!!
!! \note
!! At present, negative yy, mm, dd, ddd are __INVALID__, you have to
!! normalize it by calling normalize_yymmdd() and normalize_XXX().
!!
!! \bug epoch other than 0001 case does NOT work properly, so set_epoch() is disabled.
!!
!! \author Takahiro INOUE <tinoue@rist.jp>
!!
!! \date
!!  - 2015/05/01 (don)        Start Implementation
!!
#ifndef SECS_K
#define SECS_K 8
#endif

module mod_tdc_calen_type
  implicit none

  private ! is default
  public :: calen_t
  public :: calen_t_set_type

  public :: calen_ProGre, calen_ProGre_NL, calen_TruGre, calen_Jurian, calen_IdealM, calen_Mars

  integer,parameter,public :: secs_k = SECS_K !! tentative

  enum, bind(c)
    enumerator :: &
      & calen_ProGre, &
      & calen_ProGre_NL,  &
      & calen_TruGre, &
      & calen_Jurian, &
      & calen_IdealM
    enumerator :: calen_Mars = 14
  end enum



  !> Base abstruct class for calendar type.
  !!
  !! To use this class, you have to call calen_t_set_type() to select
  !! calendar type, then call calen_t%init() to initialize the
  !! instance.
  !! 
  type,abstract :: calen_t
    integer :: itype    = -1    !< calendar type
    integer(secs_k) :: sc_dy    = 0     !< seconds per one day (Length of Day).
    integer(secs_k) :: dy_yr    = 0     !< days per one year   (Length of year)
    integer(secs_k) :: mo_yr    = 0     !< months per one year
    integer(secs_k) :: epoch    = 1     !< epoch year
  contains
    procedure :: init     => calen_t_init  !< Initialize
    procedure :: dump     => calen_t_dump  !< Dump
    procedure :: isleap                    !< leap year ?
    procedure :: ileap                     !< isleap ? 1 : 0
!!$    procedure :: set_epoch                 !< set epoch year
    !!
    procedure :: days2yyddd                !< days -> (yy,ddd)
    procedure :: yyddd2days                !< (yy,ddd) -> days
    procedure :: yyddd2yymmdd              !< (yy,ddd) -> (yy,mm,dd)
    procedure :: yymmdd2yyddd              !< (yy,mm,dd) -> (yy,ddd)
    procedure :: yymmdd2days               !< (yy,mm,dd) -> days
    procedure :: days2yymmdd               !< days -> (yy,mm,dd)
    procedure :: normalize_yyddd           !< normalize (yy,ddd)
    procedure :: normalize_yymmdd          !< normalize (yy,mm,dd)
    procedure :: ymd_add_days 
    procedure :: ymd_sub_ymd  
    !!
    procedure :: hhmmss2secs
    procedure :: secs2hhmmss
    procedure :: hhmmss2msecs
    procedure :: msecs2hhmmss
    !!
    procedure :: get_dy_yr                 !< return dy_yr considering leap year.
    procedure :: get_ddd_mo                !< return ddd_mo for given yr and mo.
    procedure :: get_sc_dy                 !< return sc_dy
    !!
!!$    procedure :: hms_add_secs
!!$    procedure :: hms_sub_hms
  end type calen_t
 
  !> Class for Proleptic Gregorian calendar.
  type, extends(calen_t) :: calen_ProGre_t
    integer(secs_k) :: dy_mo(0:12,0:1)    !< days per each month
    integer(secs_k) :: ddd_mo(0:12,0:1)   !< days of 1st day in each month
  end type calen_ProGre_t

  !> Class for Proleptic Gregorian with NO LEAP.
  !!
  type, extends(calen_ProGre_t) :: calen_ProGre_NL_t

  end type calen_ProGre_NL_t

  !> Class for the variation of Ideal calendar,
  !! fixed days per month.
  type, extends(calen_t) :: calen_IdealM_t
    integer(secs_k) :: dy_mo      !< days per month
  end type calen_IdealM_t

  !> example Class for Mars.
  type, extends(calen_IdealM_t) :: calen_Mars_t

  end type calen_Mars_t


  character(len=80),private,save :: timestamp=TIME_STAMP



contains
  !!========================================================================
  !> set(allocate) calendar type.
  !!
  !! \note This is NOT a member routine of calen_t.
  subroutine calen_t_set_type(self,itype)
    class(calen_t),allocatable,intent(inout) :: self !< calen_t instance
    integer, intent(in) :: itype !< calendar type specifier.

    if ( allocated(self) ) deallocate(self)
    select case ( itype )
    case ( calen_ProGre )         !! Proleptic Gregorian with leap
      allocate( calen_ProGre_t::self )
    case ( calen_ProGre_NL )      !! ProGre without leap
      allocate( calen_ProGre_NL_t::self )
    case ( calen_IdealM )         !! Ideal XXX days 
      allocate( calen_idealM_t::self )
    case ( calen_Mars )
      allocate( calen_Mars_t::self )
    case default
      !! \todo error handling
      call exit(1)
    end select
  end subroutine calen_t_set_type


  !!========================================================================
  !> Initialize calen_t instance
  subroutine calen_t_init(self)
    class(calen_t),intent(inout) :: self !< calen_t instance

    select type( self )
    type is ( calen_ProGre_t )
      self%itype = calen_ProGre
      self%sc_dy = 60*60*24
      self%dy_yr = 365
      self%mo_yr = 12
      self%dy_mo = reshape(&
        &  [0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, &
        &   0, 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31],[13,2])
      self%ddd_mo = reshape(&
        &  [0,  1, 32, 60, 91,121,152,182,213,244,274,305,335, &
        &   0,  1, 32, 61, 92,122,153,183,214,245,275,306,336],[13,2])
    type is ( calen_ProGre_NL_t )
      self%itype = calen_ProGre_NL
      self%sc_dy = 60*60*24
      self%dy_yr = 365
      self%mo_yr = 12
      self%dy_mo = reshape(&
        &  [0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, &
        &   0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31],[13,2])
      self%ddd_mo = reshape(&
        &  [0,  1, 32, 60, 91,121,152,182,213,244,274,305,335, &
        &   0,  1, 32, 60, 91,121,152,182,213,244,274,305,335],[13,2])
    type is ( calen_idealM_t )
      self%itype = calen_IdealM
      self%sc_dy = 60*60*24
      self%dy_yr = 30*12
      self%mo_yr = 12
      self%dy_mo = 30 
    type is ( calen_Mars_t )
      self%itype = calen_Mars
      self%sc_dy = 88775        !! 24:39:35.244
      self%dy_yr = 687          !! 686.98 days
      self%mo_yr = 1
      self%dy_mo = self%dy_yr
    class default
      call exit(1)
    end select

    return
  end subroutine calen_t_init



  !!========================================================================
  !> Dump calen_t instance
  subroutine calen_t_dump(self,lun)
    class(calen_t),intent(in) :: self !< calen_t instance
    integer,intent(in) :: lun !< logical unit number for output



    select type( self )
    type is ( calen_ProGre_t )
      write(lun,'(A)')'========== dump: calen_ProGre_t =========='
      write(lun,'(A10,": ",I4.4)') 'epoch',self%epoch
      write(lun,'(A10,": ",I0)') 'sc/dy',self%sc_dy
      write(lun,'(A10,": ",I0)') 'dy/yr',self%dy_yr
      write(lun,'(A10,": ",I0)') 'mo/yr',self%mo_yr
      write(lun,'(A10,": ",12I4,/,12X,12I4)') 'dy/mo',self%dy_mo(1:12,:)
      write(lun,'(A10,": ",12I4,/,12X,12I4)') 'ddd@mo',self%ddd_mo(1:12,:)
    type is ( calen_ProGre_NL_t )
      write(lun,'(A)')'==========: calen_ProGre_NL_t =========='
      write(lun,'(A10,": ",I4.4)') 'epoch',self%epoch
      write(lun,'(A10,": ",I0)') 'sc/dy',self%sc_dy
      write(lun,'(A10,": ",I0)') 'dy/yr',self%dy_yr
      write(lun,'(A10,": ",I0)') 'mo/yr',self%mo_yr
      write(lun,'(A10,": ",12I4)') 'dy/mo',self%dy_mo(1:12,0)
      write(lun,'(A10,": ",12I4)') 'ddd@mo',self%ddd_mo(1:12,0)
    type is ( calen_IdealM_t )
      write(lun,'(A)')'==========: calen_IdealM_t =========='
      write(lun,'(A10,": ",I4.4)') 'epoch',self%epoch
      write(lun,'(A10,": ",I0)') 'sc/dy',self%sc_dy
      write(lun,'(A10,": ",I0)') 'dy/yr',self%dy_yr
      write(lun,'(A10,": ",I0)') 'mo/yr',self%mo_yr
      write(lun,'(A10,": ",I0)') 'dy/mo',self%dy_mo
    type is ( calen_Mars_t )
      write(lun,'(A)')'==========: calen_Mars_t =========='
      write(lun,'(A10,": ",I4.4)') 'epoch',self%epoch
      write(lun,'(A10,": ",I0)') 'sc/dy',self%sc_dy
      write(lun,'(A10,": ",I0)') 'dy/yr',self%dy_yr
      write(lun,'(A10,": ",I0)') 'mo/yr',self%mo_yr
      write(lun,'(A10,": ",I0)') 'dy/mo',self%dy_mo
    end select

    return
  end subroutine calen_t_dump
    

  !!========================================================================
  !> set epoch year
  !!
  subroutine set_epoch( self, year )
    class(calen_t),intent(inout) :: self !< calen_t instance
    integer(secs_k),intent(in) :: year !< epoch year
    
    self%epoch = year

    return

  end subroutine set_epoch



  !!========================================================================
  !> leap year or not.
  function isleap(self,yr) result (res)
    class(calen_t),intent(in) :: self
    integer(secs_k),intent(in) :: yr
    logical :: res

    select type (self)
    type is ( calen_ProGre_t )
      if      ( mod (yr,400) == 0 ) then
        res = .true.
      else if ( mod (yr,100) == 0 ) then
        res = .false.
      else if ( mod (yr,4) == 0 ) then
        res = .true.
      else
        res = .false.
      end if
    type is ( calen_ProGre_NL_t)
      res = .false.
      return
    class is ( calen_IdealM_t )
      res = .false.
      return
    class default
      write(0,*)'Invalid calendar type:',self%itype
      call exit(1)
    end select

    return

  end function isleap


  !!========================================================================
  !> return 1 if leap year
  function ileap(self,yr) result (res)
    class(calen_t),intent(in) :: self
    integer(secs_k),intent(in) :: yr
    integer(secs_k) :: res
    
    res = 0
    if ( self%isleap(yr) ) res = 1

    return

  end function ileap


  !!========================================================================
  !> return dy_yr, considering leap year
  function get_dy_yr(self,yr) result (res)
    class(calen_t),intent(in) :: self
    integer(secs_k),intent(in) :: yr
    integer(secs_k) :: res

    res = self%dy_yr+self%ileap(yr)

  end function get_dy_yr



  !!==================================================================================
  !> return ddd_mo for given yr and mo.
  function get_ddd_mo(self, yr, mo) result (res)
    class(calen_t),intent(in) :: self
    integer(secs_k),intent(in) :: yr
    integer(secs_k),intent(in) :: mo
    integer(secs_k) :: res

!!$    res = self%ddd_mo(mo,self%ileap(yy))

    select type( self )
    type is ( calen_ProGre_t )
      if ( self%isleap(yr) ) then
        res = self%ddd_mo(mo,1)
      else
        res = self%ddd_mo(mo,0)
      end if
    type is ( calen_ProGre_NL_t )
      !! \todo implement here
    class default
      write(0,*)'Invalid calendar type:',self%itype
      call exit(1)
    end select

    return

  end function get_ddd_mo



  !!========================================================================
  !> return sc_dy,
  function get_sc_dy(self) result (res)
    class(calen_t),intent(in) :: self !< calen_t instance
    integer(secs_k) :: res            !< sc_dy

    res = self%sc_dy

  end function get_sc_dy







  !!========================================================================
  !> days to yy-ddd
  !!
  !! Assumes that the EPOCH is 0001/01/01, and `days` is days since
  !! one day before the epoch.  that is, `days` of 0001-01-01 (or
  !! 0001-001) is 1.
  !!
  !! \note
  !! On return, yy-ddd is normalized, and input `days` must NOT be non-positive.
  !!
  subroutine days2yyddd( self, yy, ddd, days )
    class(calen_t),intent(in) :: self !< extended of calen_t instance
    integer(secs_k),intent(out) :: yy         !< year
    integer(secs_k),intent(out) :: ddd        !< days in the year
    integer(secs_k),intent(in)  :: days       !< total days from the epoch.

    integer(secs_k) :: i4c    !! set of 400 years 
    integer(secs_k) :: i1c    !! set of 100 years
    integer(secs_k) :: i4y    !! set of 4 years
    integer(secs_k) :: i1y    !! set of 1 year
    integer(secs_k) :: ires   !! residual

    integer(secs_k) :: d4c    !! days of 400 years 
    integer(secs_k) :: d1c    !! days of 100 years
    integer(secs_k) :: d4y    !! days of 4 years
    integer(secs_k) :: d1y    !! days of 1 year

    logical :: days_is_negative

    days_is_negative= ( days <= 0 )

    select type ( self )
    type is ( calen_ProGre_t )
      d4c = self%dy_yr*400+97
      d1c = self%dy_yr*100+24
      d4y = self%dy_yr*4+1
      d1y = self%dy_yr
      

      ires = abs(days)
      i4c  = ires/d4c
      ires = days - i4c*d4c
      i1c  = ires/d1c
      ires = ires - i1c*d1c
      i4y  = ires/d4y
      ires = ires - i4y*d4y
      i1y  = ires/d1y
      ires = ires - i1y*d1y

!!$      yy = i4c*400+i1c*100+i4y*4+i1y+1
      yy = i4c*400+i1c*100+i4y*4+i1y+self%epoch
      ddd = ires

      if ( ddd < 1 ) then
        yy = yy-1
        ddd = ddd + self%dy_yr + self%ileap(yy)
      end if

      if ( days_is_negative ) yy = -yy

    type is ( calen_ProGre_NL_t )
!!$      yy = days/self%dy_yr+1
      yy = days/self%dy_yr+self%epoch
      ddd = mod(days,self%dy_yr)
      if ( ddd < 1 ) then
        yy = yy-1
        ddd = ddd + self%dy_yr
      end if

      if ( days_is_negative ) yy = -yy

    class is ( calen_IdealM_t )
!!$      yy = days/self%dy_yr+1
      yy = days/self%dy_yr+self%epoch
      ddd = mod(days,self%dy_yr)
      if ( ddd < 1 ) then
        yy = yy-1
        ddd = ddd + self%dy_yr
      end if

      if ( days_is_negative ) yy = -yy

    class default
      call exit(1)
    end select


    return


  end subroutine days2yyddd



  !!========================================================================
  !> normalize yr-ddd
  !!
  !! if `abs(ddd)>dy_yr`, `yr` is incremented/decremented and `ddd` is
  !! assured to be `0 < ddd <= dy_yr`.
  !! 
  subroutine normalize_yyddd( self, yy, ddd )
    class(calen_t),intent(in) :: self
    integer(secs_k),intent(inout) :: yy
    integer(secs_k),intent(inout) :: ddd !< days in the year

    integer(secs_k) :: y0 !! to preserve yy
    integer(secs_k) :: d0 !! to preserve ddd
    integer(secs_k) :: y1 !! correction for yy
    integer(secs_k) :: d1 !! correction for ddd

    !! preserve input values
    y0 = yy  
    d0 = ddd

    select type (self)
    class is ( calen_ProGre_t )
      do
        if ( d0 > (self%get_dy_yr(y0) ) ) then
          y1 = +1
          d1 = -self%get_dy_yr(y0)
        else if ( d0 < 1 ) then
          y1 = -1
          d1 = self%get_dy_yr(y0-1) !! note that the year before.
        else
          y1 = 0
          d1 = 0
        end if
!!$        write(*,*)'dbg:y1,d1:',y1,d1
        if ( y1 == 0 ) exit
        y0 = y0 + y1
        d0 = d0 + d1
     end do
      yy = y0
      ddd = d0
      return
    class default
      call exit(9) ! tentative
    end select

    return

  end subroutine normalize_yyddd



  !!========================================================================
  !> (yy,ddd) to (yy,mm,dd)
  !!
  !! `yy` is unchanged.
  !! `ddd` must be  NORMALIZED in caller.
  !!
  subroutine yyddd2yymmdd( self, yy, mm, dd, ddd )
    class(calen_t),intent(in) :: self
    integer(secs_k),intent(in) :: yy
    integer(secs_k),intent(out) :: mm
    integer(secs_k),intent(out) :: dd
    integer(secs_k),intent(in)  :: ddd

    integer(secs_k) :: i

    select type( self )
    class is ( calen_ProGre_t )
      do i = self%mo_yr, 1, -1
        if ( self%get_ddd_mo(yy,i) <= ddd ) then
          mm = i
          dd = ddd - self%get_ddd_mo(yy,i) +1
          exit
        end if
      end do
    type is ( calen_idealM_t )
      mm = ddd/self%dy_mo + 1
      dd = mod(ddd,self%dy_mo)
    class default
      call exit(1)
    end select

    return

  end subroutine yyddd2yymmdd


  !!========================================================================
  !> days to (yy,mm,dd)
  subroutine days2yymmdd( self, yy, mm, dd, days )
    class(calen_t),intent(in) :: self
    integer(secs_k),intent(out) :: yy
    integer(secs_k),intent(out) :: mm
    integer(secs_k),intent(out) :: dd
    integer(secs_k),intent(in)  :: days

    integer(secs_k) :: ddd

    !! days -> yy-ddd
    call self%days2yyddd (yy,ddd,days)   !! days2yyddd() returns NORMALIZED ddd.
    !! yy-ddd -> yy-mm-dd
    call self%yyddd2yymmdd( yy, mm, dd, ddd )

    return
    
  end subroutine days2yymmdd


  !!========================================================================
  !> (yy,mm,dd) to (yy,ddd)
  !!
  !! \note yy in NOT preserved when normalization of yy-mm-dd is needed.
  !!
  subroutine yymmdd2yyddd( self, ddd, yy, mm, dd )
    class(calen_t),intent(in) :: self
    integer(secs_k),intent(out)  :: ddd
    integer(secs_k),intent(inout) :: yy
    integer(secs_k),intent(in) :: mm
    integer(secs_k),intent(in) :: dd

    integer(secs_k) :: yy0, mm0, dd0 !! preserve input

    integer(secs_k) :: y1

    integer(secs_k) :: m0 ! mod(mm, mo_yr)


    yy0 = yy;    mm0 = mm;     dd0 = dd

    !! Normalize yy and mm.
    m0 = mod( mm0,self%mo_yr )
    y1 = mm0/self%mo_yr
    if ( m0 < 1 ) then
      m0 = m0 + self%mo_yr
      y1 = y1 - 1
    end if
    !! at this point, 0 < m0 <= mo_yr
    mm0 = m0
    yy0 = yy0 + y1

    !! then yy-mm-dd to yy-ddd
    select type( self )
    class is ( calen_ProGre_t )
      ddd = dd0 + self%get_ddd_mo(yy0,mm0) - 1 !! ddd: days in the year.
      call self%normalize_yyddd(yy0,ddd)       !! normalize ddd
    type is ( calen_idealM_t )
      ddd = (mm0-1)*self%dy_mo + dd0
    class default
      call exit(1)
    end select

    yy = yy0

  end subroutine yymmdd2yyddd


  !!========================================================================
  !> (yy,ddd) to days
  !!
  !! `yy` is unchanged.
  !! `ddd` must be  NORMALIZED in caller.
  !!
  !!
  subroutine yyddd2days( self, days, yy, ddd )
    class(calen_t),intent(in) :: self
    integer(secs_k),intent(out) :: days
    integer(secs_k),intent(in)  :: yy
    integer(secs_k),intent(in)  :: ddd

    integer(secs_k) :: i4c    !! set of 400 years 
    integer(secs_k) :: i1c    !! set of 100 years
    integer(secs_k) :: i4y    !! set of 4 years
    integer(secs_k) :: i1y    !! set of 1 year
    integer(secs_k) :: ires   !! residual


    select type ( self )
    type is ( calen_ProGre_t )
!!$      ires = yy-1
      ires = yy-self%epoch
      i4c = ires/400
      ires = ires-i4c*400
      i1c = ires/100
      ires = ires - i1c*100
      i4y = ires/4
      ires = ires - i4y*4
      i1y = ires

!!$      write(0,*)'dbg:yyddd2days:',i4c, i1c, i4y, i1y

      days = i4c*(self%dy_yr*400+97) &
        &   +i1c*(self%dy_yr*100+24) &
        &   +i4y*(self%dy_yr*4+1) &
        &   +i1y*self%dy_yr&
        &   +ddd
    type is ( calen_ProGre_NL_t )
      days = (yy-1)*self%dy_yr+ddd
    class default
      call exit(1)
    end select

    return

  end subroutine yyddd2days


  !!========================================================================
  !> (yy,mm,dd) to days in one stop.
  !!
  !! \caution Input is UNCHANGED.
  !!
  subroutine yymmdd2days( self, days, yy, mm, dd )
    class(calen_t),intent(in) :: self
    integer(secs_k),intent(out)  :: days
    integer(secs_k),intent(in) :: yy
    integer(secs_k),intent(in) :: mm
    integer(secs_k),intent(in) :: dd
    
    integer(secs_k) :: yy0, mm0, dd0 !! to preserve input
    integer(secs_k) :: ddd

    yy0 = yy;    mm0 = mm;     dd0 = dd

    call self%yymmdd2yyddd(ddd,yy0,mm0,dd0) !! yy0 may changed.

    call self%yyddd2days(days,yy0,ddd)

    return

  end subroutine yymmdd2days


  !!========================================================================
  !> Normalize yr-mo-dy
  subroutine normalize_yymmdd( self, yy, mm, dd )
    class(calen_t),intent(in) :: self
    integer(secs_k),intent(inout) :: yy
    integer(secs_k),intent(inout) :: mm
    integer(secs_k),intent(inout) :: dd

    integer(secs_k) :: ddd
    
    call self%yymmdd2yyddd(ddd,yy,mm,dd)

    call self%yyddd2yymmdd(yy,mm,dd,ddd)

    return

  end subroutine normalize_yymmdd


  !!========================================================================
  !> add days to ymd.
  function ymd_add_days(self,ymd,days) result ( res )
    class(calen_t),intent(in) :: self
    integer(secs_k),intent(in) :: ymd(3) !< [yy,mm,dd]
    integer(secs_k),intent(in) :: days   !< days
    integer(secs_k) :: res(3)

    integer(secs_k) ::d

    call self%yymmdd2days(d,ymd(1),ymd(2),ymd(3))
    call self%days2yymmdd(res(1),res(2),res(3),days+d)

    return
    
  end function ymd_add_days

  !!========================================================================
  !> subtract ymd from ymd.
  function ymd_sub_ymd(self,ymd2,ymd1) result ( res )
    class(calen_t),intent(in) :: self
    integer(secs_k),intent(in) :: ymd2(3) !< [yy,mm,dd]
    integer(secs_k),intent(in) :: ymd1(3) !< [yy,mm,dd]
    integer(secs_k) :: res !< days

    integer(secs_k) ::d1,d2

    call self%yymmdd2days(d1,ymd1(1),ymd1(2),ymd1(3))
    call self%yymmdd2days(d2,ymd2(1),ymd2(2),ymd2(3))
    res = d2-d1

    return
    
  end function ymd_sub_ymd







  
  !!========================================================================
  !> convert (hh,mm,ss) to secs.
  !!
  !! Input hh:mm:ss must be NORMALIZED BEFORE.
  !!
  subroutine hhmmss2secs( self, secs, hr, mn, sc )
    class(calen_t),intent(inout) :: self !< calen_t instance
    integer(secs_k),intent(out) :: secs !< total seconds
    integer(secs_k),intent(in)  :: hr   !< hr
    integer(secs_k),intent(in)  :: mn   !< mn
    integer(secs_k),intent(in)  :: sc   !< sc

    secs =  hr * 60 * 60 + mn * 60 + sc

    return

  end subroutine hhmmss2secs


  !!========================================================================
  !> convert secs to (hh,mm,ss) 
  subroutine secs2hhmmss( self, hr, mn, sc, secs )
    class(calen_t),intent(inout) :: self !< calen_t instance
    integer(secs_k),intent(out) :: hr   !< hr
    integer(secs_k),intent(out) :: mn   !< mn
    integer(secs_k),intent(out) :: sc   !< sc
    integer(secs_k),intent(in)  :: secs !< total seconds


    if ( ( secs < 0 ) .or. ( secs > self%sc_dy ) ) then
      write(0,*)'calen_t_secs2hhmmss: Invalid secs:',secs
      call exit(1)
    end if

    hr = secs/(60*60)
    mn = (secs-hr*60*60)/60
    sc = mod( secs, 60 )
    
    return
  end subroutine secs2hhmmss


  !!========================================================================
  !> convert (hr,mn,sc,ms) to msecs
  !!
  !! If msecs is out of sc_dy range, carry to/bollow from days by `dc`.
  subroutine hhmmss2msecs( self, msecs, hr, mn, sc, ms )
    class(calen_t),intent(inout) :: self !< calen_t instance
    integer(secs_k),intent(out) :: msecs !< total milli-seconds
    integer(secs_k),intent(in)  :: hr   !< hr
    integer(secs_k),intent(in)  :: mn   !< mn
    integer(secs_k),intent(in)  :: sc   !< sc
    integer(secs_k),intent(in)  :: ms   !< ms

    integer(secs_k) :: secs

    call self%hhmmss2secs( secs, hr, mn, sc )
    msecs = secs * 1000 + ms               

    return

  end subroutine hhmmss2msecs


  !!========================================================================
  !> convert msecs to (hh,mm,ss) 
  subroutine msecs2hhmmss( self, hr, mn, sc, ms, msecs )
    class(calen_t),intent(inout) :: self !< calen_t instance
    integer(secs_k),intent(out) :: hr   !< hr
    integer(secs_k),intent(out) :: mn   !< mn
    integer(secs_k),intent(out) :: sc   !< sc
    integer(secs_k),intent(out) :: ms   !< ms
    integer(secs_k),intent(in)  :: msecs !< total seconds


    integer(secs_k) :: secs

    ms = mod(msecs,1000)

    secs = (msecs-ms)/1000
    call self%secs2hhmmss( hr, mn, sc, secs )
    
    return
  end subroutine msecs2hhmmss



  !!========================================================================
  !> add sec to hms
!!$  function hms_add_secs( self, hms, secs ) result (res)
!!$    class(calen_t),intent(inout) :: self
!!$    integer(secs_k),intent(in) :: hms(3) !< [hh,mm,ss]
!!$    integer(secs_k),intent(in) :: secs !< secs
!!$    integer(secs_k) :: res(3) !< [hh,mm,ss]
!!$
!!$    integer(secs_k) :: s
!!$    integer(secs_k) :: dc
!!$
!!$
!!$       !! call self%hhmmss2secs(s,dc, hms(1),hms(2),hms(3))
!!$    s =  hr * 60 * 60 + mn * 60 + sc
!!$    call self%secs2hhmmss(res(1),res(2),res(3),secs+s)
!!$
!!$    return
!!$
!!$  end function hms_add_secs

  !!========================================================================
  !> hms - hms in secs
!!$  function hms_sub_hms( self, hms2, hms1 ) result ( res )
!!$    class(calen_t),intent(inout) :: self
!!$    integer(secs_k),intent(in) :: hms2(3) !< [hh,mm,ss]
!!$    integer(secs_k),intent(in) :: hms1(3) !< [hh,mm,ss]
!!$    integer(secs_k) :: res !< secs
!!$
!!$    integer(secs_k) :: s1, s2
!!$
!!$    call self%hhmmss2secs( s1,hms1(1),hms1(2),hms1(3) )
!!$    call self%hhmmss2secs( s2,hms2(1),hms2(2),hms2(3) )
!!$    res = s2 - s1
!!$
!!$    return
!!$
!!$  end function hms_sub_hms


end module mod_tdc_calen_type
  





!$========================================================================
#ifdef TEST
!$========================================================================
program test_tdc_calen_type
  use mod_tdc_calen_type
  implicit none
  
  integer :: itest
  character(len=2) :: ctest

  integer :: lun = 6


  class(calen_t),allocatable  :: cal
  integer :: i

  integer(secs_k) :: yrs(24)
  logical :: lps(24)

  integer,parameter :: num=20
  integer(secs_k) :: yr(num)
  integer(secs_k) :: mo(num)
  integer(secs_k) :: dy(num)
  integer(secs_k) :: ddd(num)
  integer(secs_k) :: days(num)

  integer(secs_k) :: hr(num)
  integer(secs_k) :: mn(num)
  integer(secs_k) :: sc(num)
  integer(secs_k) :: ms(num)
  integer(secs_k) :: sec(num)
  integer(secs_k) :: msec(num)
  integer :: n
  
  integer(secs_k) :: d

  integer(secs_k) :: ss

  character(len=*),parameter :: form_cdate='(I6.4,"-",I2.2,"-",I2.2)'
  character(len=*),parameter :: form_ctime='(I2.2,":",I2.2,":",I2.2)'
  character(len=*),parameter :: form_ctime_ms='(I2.2,":",I2.2,":",I2.2,".",I3.3)'


  if ( command_argument_count() < 1 ) then
    itest = 1
  else
    call get_command_argument(1,ctest)
    read(ctest,*) itest
  end if
  write(*,'(A,I0)')'itest: ',itest


  select case ( itest )
  case ( 1 )
    call calen_t_set_type(cal,calen_ProGre)
  case ( 2 )
    call calen_t_set_type(cal,calen_ProGre_NL)
  case ( 3 )
    call calen_t_set_type(cal,calen_IdealM)
  case ( 4 )
    call calen_t_set_type(cal,calen_Mars)
  case default
    call exit(1)
  end select

  call cal%init()
!!$  call cal%set_epoch(2001_secs_k)
  call cal%dump(lun)

  write(lun,*)'========== leap year =========='
  n = 24
  yrs(:) = [ (100*i,i=1,n) ]
  do i=1,n
    lps(i) = cal%isleap(yrs(i))
  end do

  write(lun,'(12I5.4)')yrs(1:12)
  write(lun,'(12L5)  ')lps(1:12)
  write(lun,'(12I5.4)')yrs(13:24)
  write(lun,'(12L5)  ')lps(13:24)

  yrs(:) = [ (1992+i,i=1,24) ]
  do i=1,24
    lps(i) = cal%isleap(yrs(i))
  end do
  write(lun,'(12I5.4)')yrs(1:12)
  write(lun,'(12L5)'  )lps(1:12)
  write(lun,'(12I5.4)')yrs(13:24)
  write(lun,'(12L5)'  )lps(13:24)

  
  write(lun,*)'========== normalize_yymmdd =========='
  n = 11
  yr(1:n)=[2001,2001,2000,2001,2001,2001,2015,2015,2015,2015,2015]
  mo(1:n)=[   0,   6,   3,   3,  12,  -2, -10,  26, -23,   5,   5]
  dy(1:n)=[   1,   0,   0,   0, -25,  29, -10,  12,  25,-400, 400]
  do i = 1, n
    write(lun,'(I6,",",I6,",",I6," -> ")',advance='no')yr(i), mo(i), dy(i)
    call cal%normalize_yymmdd( yr(i), mo(i), dy(i) )
    write(lun,'(I6,",",I6,",",I6)')yr(i), mo(i), dy(i)
  end do


  write(lun,*)'========== yymmdd <-> days =========='
  n = 7
  yr(1:n)=[1,1900,1901,2001,2015,2020,2015]
  mo(1:n)=[1,   1,   1,   1,   5,  12,   2]
  dy(1:n)=[1,   1,   1,   1,   8,  25,  29]
  do i = 1, n
    call cal%yymmdd2days(days(i),yr(i),mo(i),dy(i))
    write(lun,'(A14,'//form_cdate//'" ->",I12)')  'yymmdd2days:',yr(i),mo(i),dy(i),days(i)
    call cal%days2yymmdd(yr(i),mo(i),dy(i),days(i))
    write(lun,'(A14,I12," ->"'//form_cdate//')')  'days2yymmdd:',days(i),yr(i),mo(i),dy(i)
  end do
  write(lun,*)

  
  n = 7
  days(1:n)=[365,1461,36524,146097,100000,500000,1000000]
  do i = 1, n
    call cal%days2yymmdd(yr(i),mo(i),dy(i),days(i))
    write(lun,'(A14,I12," ->"'//form_cdate//')')'days2yymmdd:',days(i),yr(i),mo(i),dy(i)
    call cal%yymmdd2days(days(i),yr(i),mo(i),dy(i))
    write(lun,'(A14,'//form_cdate//'" ->",I12)')'yymmdd2days:',yr(i),mo(i),dy(i),days(i)
  end do
  write(lun,*)

  write(lun,*)'========== yyddd <-> days =========='
  n = 5
  yr(1:n) =[1,1900,2001,2015,2015]
  ddd(1:n)=[1,   1,   1,  32, 366]
  do i=1,n
    call cal%yyddd2days(days(i),yr(i),ddd(i))
    write(lun,'(A14,I6.4,"-",I3.3," ->",I10)')'yyddd2days:',yr(i),ddd(i),days(i)
    call cal%days2yyddd(yr(i),ddd(i),days(i))
    write(lun,'(A14,I10," ->",I6.4,"-",I3.3)')'days2yyddd:',days(i),yr(i),ddd(i)
  end do

  write(lun,*)

  n = 4
  days(1:n)=[693596,730486,735726,737784]
  do i=1,n
    call cal%days2yyddd(yr(i),ddd(i),days(i))
    write(lun,'(A14,I10," ->",I6.4,"-",I3.3)')'days2yyddd:',days(i),yr(i),ddd(i)
    call cal%yyddd2days(days(i),yr(i),ddd(i))
    write(lun,'(A14,I6.4,"-",I3.3," ->",I10)')'yyddd2days:',yr(i),ddd(i),days(i)
  end do
  write(lun,*)

  write(lun,*)'========== ymd + days =========='
  n = 6
  yr(1:n)=[1,1900,2001,2015,2020,2015]
  mo(1:n)=[1,   1,   1,   5,  12,   2]
  dy(1:n)=[1,   1,   1,   8,  25,  29]
  do i = 1, n
    write(lun,'(A14,'//form_cdate//'" -> "'//form_cdate//')')&
      &'ymd_add_days:',    [yr(i),mo(i),dy(i)], &
      &  cal%ymd_add_days( [yr(i),mo(i),dy(i)], 40_secs_k )
  end do
  write(lun,*)

  n = 6
  yr(1:n)=[ 1,1900,2001,2015,2021,2015]
  mo(1:n)=[ 2,   2,   2,   6,   2,   4]
  dy(1:n)=[10,  10,  10,  17,   3,  10]
  do i = 1, n
    write(lun,'(A14,'//form_cdate//'" -> "'//form_cdate//')')&
      &'ymd_add_days:',    [yr(i),mo(i),dy(i)], &
      &  cal%ymd_add_days( [yr(i),mo(i),dy(i)], -40_secs_k )
  end do
  write(lun,*)
  


  write(lun,*)'========== ymd - ymd =========='
  n = 6
  yr(1:n)=[1,1900,2001,2015,2020,2015]
  mo(1:n)=[1,   1,   1,   5,  12,   2]
  dy(1:n)=[1,   1,   1,   8,  25,  29]
  do i=1,n
    d = cal%ymd_sub_ymd( [yr(i+1),mo(i+1),dy(i+1)], [yr(i),mo(i),dy(i)] )
    write(lun,'(A14,'//form_cdate//'" -"'//form_cdate//'" =",I8)')'ymd_sub_ymd:',yr(i+1),mo(i+1),dy(i+1), yr(i),mo(i),dy(i), d
  end do
  write(lun,*)


  write(lun,*)'========== hhmmss <-> secs, msecs =========='
  n = 5
  hr(1:n)=[ 0,  6, 18, 23, 24]
  mn(1:n)=[ 0, 18, 56, 56, 00]
  sc(1:n)=[ 0, 25, 04, 41, 00]
  do i=1,n
    call cal%hhmmss2secs(sec(i), hr(i), mn(i), sc(i))
    write(lun,'(A14,'//form_ctime//'" -> ",I8)')'hhmmss2secs: ',hr(i),mn(i),sc(i),sec(i)
    call cal%secs2hhmmss(hr(i), mn(i), sc(i), sec(i))
    write(lun,'(A14,I8," -> "'//form_ctime//')')'secs2hhmmss: ',sec(i),hr(i),mn(i),sc(i)
  end do
  write(lun,*)

  n = 6
  sec(1:n) = [ 0,   4000, 12345, 86400, 100000, -7230 ]
  do i=1,n
    call cal%secs2hhmmss(hr(i), mn(i), sc(i), sec(i))
    write(lun,'(A14,I8," -> "'//form_ctime//')')'secs2hhmmss: ',sec(i),hr(i),mn(i),sc(i)
    call cal%hhmmss2secs(sec(i), hr(i), mn(i), sc(i))
    write(lun,'(A14,'//form_ctime//'" -> ",I8)')'hhmmss2secs: ',hr(i),mn(i),sc(i),sec(i)
  end do
  write(lun,*)



  n = 6
  hr(1:n)=[ 0,  6, 18, 23, 24]
  mn(1:n)=[ 0, 18, 56, 56, 00]
  sc(1:n)=[ 0, 25, 04, 41, 00]
  ms(1:n)=[ 0, 10,200,567,999] 
  do i=1,n
    call cal%hhmmss2msecs(sec(i), hr(i), mn(i), sc(i), ms(i))
    write(lun,'(A14,'//form_ctime_ms//'" -> ",I12)')'hhmmss2msecs: ',hr(i),mn(i),sc(i),ms(i),sec(i)
    call cal%msecs2hhmmss(hr(i), mn(i), sc(i), ms(i), sec(i))
    write(lun,'(A14,I12," -> "'//form_ctime_ms//')')'msecs2hhmmss: ',sec(i),hr(i),mn(i),sc(i),ms(i)
  end do
  write(lun,*)


  n = 1
  msec(1:n) = [ 1000000 ]
  do i=1,n
    call cal%msecs2hhmmss(hr(i), mn(i), sc(i), ms(i), msec(i))
    write(lun,'(A14,I12," -> ",I2.2,":",I2.2,":",I2.2,".",I3.3)')'msecs2hhmmss: ',msec(i),hr(i),mn(i),sc(i),ms(i)
    call cal%hhmmss2msecs(msec(i), hr(i), mn(i), sc(i), ms(i))
    write(lun,'(A14,I2.2,":",I2.2,":",I2.2,".",I3.3," -> ",I12)')'hhmmss2msecs: ',hr(i),mn(i),sc(i),ms(i), msec(i)
  end do



!!$  write(lun,*)'add_secs',cal%hms_add_secs([hr,mn,sc],150)
!!$  ss = 198
!!$  write(lun,*)'========== hms +',ss,' secs =========='
!!$  n = 4
!!$  hr(1:n)=[0,  6, 12, 23]
!!$  mn(1:n)=[0, 18, 51, 59]
!!$  sc(1:n)=[0, 34, 12, 45]
!!$  do i = 1, n
!!$    write(lun,'(A14,'//form_ctime//'" -> "'//form_ctime//')')&
!!$      &'hms_add_secs:',    [hr(i),mn(i),sc(i)], &
!!$      &  cal%hms_add_secs( [hr(i),mn(i),sc(i)], ss )
!!$  end do
!!$  write(lun,*)
!!$
!!$  ss = -198
!!$  write(lun,*)'========== hms +',ss,' secs =========='
!!$  n = 4
!!$  hr(1:n)=[0,  6, 12, 23]
!!$  mn(1:n)=[0, 18, 51, 59]
!!$  sc(1:n)=[0, 34, 12, 45]
!!$  do i = 1, n
!!$    write(lun,'(A14,'//form_ctime//'" -> "'//form_ctime//')')&
!!$      &'hms_add_secs:',    [hr(i),mn(i),sc(i)], &
!!$      &  cal%hms_add_secs( [hr(i),mn(i),sc(i)], ss )
!!$  end do
!!$  write(lun,*)



  call exit(0)

end program test_tdc_calen_type
!$========================================================================
#endif
!$========================================================================



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
  
