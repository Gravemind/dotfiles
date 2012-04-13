# Makefile for xtrlock - X Transparent Lock
# This Makefile provided for those of you who lack a functioning xmkmf.
#
# Copyright (C)1993,1994 Ian Jackson
#
# This is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# This is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

LDLIBS=-lX11 -lcrypt
CC=gcc
CFLAGS=-Wall -DSHADOW_PWD=1
INSTALL=install

xtrlock:	xtrlock.o

xtrlock.o:	xtrlock.c lock.bitmap mask.bitmap patchlevel.h

install:	xtrlock
		$(INSTALL) -c -m 755 xtrlock /usr/bin/xtrlock
		chmod u+s /usr/bin/xtrlock

install.man:
		$(INSTALL) -c -m 644 xtrlock.man /usr/man/man1/xtrlock.1x
