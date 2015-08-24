#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])
AC_INIT([jcup_util], [0.1], [tinoue@rist.jp])
AC_PREFIX_DEFAULT([$HOME/jcup_util])
AC_CONFIG_SRCDIR([src/mod_tdc.F03])

# Checks for programs.
AC_PROG_INSTALL
AC_PROG_MKDIR_P
AC_PROG_LN_S
AC_CHECK_TOOL([RANLIB], [ranlib], [:])
RM="rm -f"
AR=ar
ARFLAGS=ruv
AC_SUBST(RM)
AC_SUBST(AR)
AC_SUBST(ARFLAGS)

## Checks for C compiler.
## mainly for yyCIO.c
AC_LANG_PUSH([C])
AC_PROG_CC([ccpx icc gcc])
dnl AX_MPI
AC_LANG_POP([C])

## Checks for Fortran compiler.
FCFLAGS_preserve=$FCFLAGS
AC_LANG(Fortran)
AC_PROG_FC([frtpx ifort gfortran])

case "$FC" in 
  gfortran*)
    gfortran --version | head -1 | grep -v 4.8.0 || AC_MSG_ERROR([Do NOT use gfortran 4.8.0. Use 4.8.1 or higher. See http://gcc.gnu.org/bugzilla/show_bug.cgi?id=56696 .])
  ;;
  *)
  ;;
esac

AC_MSG_CHECKING([FC])
AC_MSG_RESULT($FC)


FCFLAGS=$FCFLAGS_preserve
## FCFLAGS 
AC_MSG_CHECKING([FCFLAGS])
if test -z "$FCFLAGS"; then
case "$FC" in
  gfortran*)       
    FCFLAGS="-std=gnu -g -fbounds-check -fbacktrace -O3 -funroll-loops -pipe -ffpe-trap=zero,overflow -fimplicit-none -fno-automatic -fconvert=big-endian -frecord-marker=4"
  ;;
  ifort)
    FCFLAGS="-O3 -warn all -openmp -openmp-report2 -parallel -par-report1"
  ;;
  frtpx)
    FCFLAGS="-Kfast,parallel,auto,ocl,preex,array_private,noalias=s,mfunc=2 \
               -Kprefetch_cache_level=all,prefetch_iteration_L2=50 \
               -Kdynamic_iteration -Ksimd -Fwide \
               -Qi -Qt -X03 -Ncompdisp -Koptmsg=1"
  ;;
  *)
    FCFLAGS="-O2"
  ;;
esac
fi
AC_MSG_RESULT($FCFLAGS)


### MPI
dnl AX_MPI


# Checks for libraries.


# Checks for header files.


# Checks for typedefs, structures, and compiler characteristics.


# Checks for library functions.
#AC_FUNC_MALLOC
#AC_CHECK_FUNCS([strchr])


### defines

AC_CONFIG_FILES([Makefile] )
AC_CONFIG_FILES([src/Makefile] )
AC_CONFIG_FILES([doc/Makefile] )
AC_CONFIG_FILES([stamp-h], [echo timestamp > stamp-h])


########################################
##### SUB DIRECTORIES
########################################



########################################
# That's it.
########################################
AC_OUTPUT
