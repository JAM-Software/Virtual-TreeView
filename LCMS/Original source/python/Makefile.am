#
# Makefile for building LCMS Python scripting extension
# Written by Bob Friesenhahn, June 2003
#

AM_CPPFLAGS =  -I$(top_builddir)/include -I$(top_srcdir)/include

pkgdir = $(pyexecdir)

# Shared libraries built in this directory
if SUPPORT_PYTHON
LCMSWRAPPER=lcms.py
LCMSBUILTSOURCES=lcms_wrap.cxx lcms.py
pkg_LTLIBRARIES = _lcms.la
else
LCMSWRAPPER=
LCMSBUILTSOURCES=
pkg_LTLIBRARIES =
endif

_lcms_la_LDFLAGS = -no-undefined -module -avoid-version \
                    -L/usr/lib/python$(PYTHON_VERSION)/config \
                    -L/usr/local/lib/python$(PYTHON_VERSION)/config 

_lcms_la_LIBADD = $(top_builddir)/src/liblcms.la -lpython$(PYTHON_VERSION)

_lcms_la_SOURCES = lcms_wrap.cxx

_lcms_la_CPPFLAGS = -I$(PYTHON_PREFIX)/include/python$(PYTHON_VERSION) \
                    -I/usr/include/python$(PYTHON_VERSION) \
                    -I/usr/local/include/python$(PYTHON_VERSION) \
                    $(AM_CPPFLAGS)

_lcms_la_DEPENDENCIES = lcms.i

BUILT_SOURCES = $(LCMSBUILTSOURCES)

EXTRA_DIST = lcms.i testbed

pkg_DATA = $(LCMSWRAPPER)

lcms_wrap.cxx: lcms.i
	$(SWIG) $(AM_CPPFLAGS) -python -shadow -c++ -I$(srcdir)  -o $@ $<

lcms.py: lcms_wrap.cxx

clean-local:
	rm -f lcms_wrap.cxx lcms.py lcms.pyc

