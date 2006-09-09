#
# If libtiff.a is installed in /usr/lib or /usr/local/lib just point
# LIBTIFF_DIR there.  It doesn't need a full libtiff tree.
#
LIBTIFF_DIR =	../../libtiff
#
CC	=	gcc
CFLAGS	=	-O -I$(LIBTIFF_DIR)
LIBS	=	$(LIBTIFF_DIR)/libtiff.a -lm

addtiffo:	addtiffo.o tif_overview.o tif_ovrcache.o
	$(CXX) $(CFLAGS) addtiffo.o tif_overview.o tif_ovrcache.o \
		$(LIBS) -o addtiffo 


addtiffo.o:	addtiffo.c
	$(CC) -c $(CFLAGS) addtiffo.c

tif_overview.o:	tif_overview.c
	$(CC) -c $(CFLAGS) tif_overview.c

tif_ovrcache.o:	tif_ovrcache.c
	$(CC) -c $(CFLAGS) tif_ovrcache.c

clean:
	rm *.o addtiffo
