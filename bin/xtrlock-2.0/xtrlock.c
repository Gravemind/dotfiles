/*
 * xtrlock.c
 *
 * X Transparent Lock
 *
 * Copyright (C)1993,1994 Ian Jackson
 *
 * This is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
 * GNU General Public License for more details.
 */

#include <X11/X.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#include <X11/Xos.h>

#define _XOPEN_SOURCE

#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <pwd.h>
#include <grp.h>
#include <limits.h>
#include <string.h>
#include <crypt.h>
#include <unistd.h>
#include <math.h>
#include <ctype.h>
#include <values.h>

#ifdef SHADOW_PWD
#include <shadow.h>
#endif

#include "lock.bitmap"
#include "mask.bitmap"
#include "patchlevel.h"

Display *display;
Window window, root;

#define TIMEOUTPERATTEMPT 30000
#define MAXGOODWILL	 (TIMEOUTPERATTEMPT*5)
#define INITIALGOODWILL MAXGOODWILL
#define GOODWILLPORTION 0.3

struct passwd *pw;
int passwordok(const char *s) {
#if 0
	char key[3];
	char *encr;

	key[0] = *(pw->pw_passwd);
	key[1] =	(pw->pw_passwd)[1];
	key[2] =	0;
	encr = crypt(s, key);
	return !strcmp(encr, pw->pw_passwd);
#else
	/* simpler, and should work with crypt() algorithms using longer
	   salt strings (like the md5-based one on freebsd).	--marekm */
	char* sss = crypt(s, pw->pw_passwd);
	return !strcmp(sss, pw->pw_passwd);
#endif
}

int main(int argc, char **argv){
	XEvent ev;
	KeySym ks;
	char cbuf[10], rbuf[128]; /* shadow appears to suggest 127 a good value here */
	int clen, rlen=0;
	long goodwill= INITIALGOODWILL, timeout= 0;
	XSetWindowAttributes attrib;
	Cursor cursor[2], cursor_warn[2], cursor_err[2];
    char   cursor_not_safe = 0;
	Pixmap csr_source,csr_mask;
	XColor csr_fg, csr_bg, dummy, csr_warn, csr_err, csr_caps;
	int ret;
#ifdef SHADOW_PWD
	struct spwd *sp;
#endif

	if (argc != 1) {
		fprintf(stderr,"xtrlock (version %s): no arguments allowed\n",program_version);
		exit(1);
	}

	errno=0;	pw = getpwuid(getuid());
	if (!pw) { perror("password entry for uid not found"); exit(1); }
#ifdef SHADOW_PWD
	sp = getspnam(pw->pw_name);
	if (sp)
		pw->pw_passwd = sp->sp_pwdp;
	endspent();
#endif

	/* logically, if we need to do the following then the same
	   applies to being installed setgid shadow.
	   we do this first, because of a bug in linux. --jdamery */
	setgid(getgid());
	/* we can be installed setuid root to support shadow passwords,
	   and we don't need root privileges any longer.	--marekm */
	setuid(getuid());

	if (strlen(pw->pw_passwd) < 13) {
		fputs("password entry has no pwd\n",stderr); exit(1);
	}

	display= XOpenDisplay(0);

	if (display==NULL) {
		fprintf(stderr,"xtrlock (version %s): cannot open display\n",
				program_version);
		exit(1);
	}

	attrib.override_redirect= True;
	window= XCreateWindow(display,DefaultRootWindow(display),
						  0,0,1,1,0,CopyFromParent,InputOnly,CopyFromParent,
						  CWOverrideRedirect,&attrib);

	XSelectInput(display,window,KeyPressMask|KeyReleaseMask);

	csr_source= XCreateBitmapFromData(display,window,lock_bits,lock_width,lock_height);
	csr_mask= XCreateBitmapFromData(display,window,mask_bits,mask_width,mask_height);

    ret = 1;
	ret = XAllocNamedColor(display,
						   DefaultColormap(display, DefaultScreen(display)),
						   "steelblue3",
						   &dummy, &csr_bg);
	if (ret==0)
		XAllocNamedColor(display,
						 DefaultColormap(display, DefaultScreen(display)),
						 "black",
						 &dummy, &csr_bg);

	ret = XAllocNamedColor(display,
						   DefaultColormap(display,DefaultScreen(display)),
						   "grey25",
						   &dummy, &csr_fg);
	if (ret==0)
		XAllocNamedColor(display,
						 DefaultColormap(display, DefaultScreen(display)),
						 "white",
						 &dummy, &csr_bg);

	ret = XAllocNamedColor(display,
						   DefaultColormap(display,DefaultScreen(display)),
						   "darkgoldenrod2",
						   &dummy, &csr_warn);

	ret = XAllocNamedColor(display,
						   DefaultColormap(display,DefaultScreen(display)),
						   "firebrick",
						   &dummy, &csr_err);

	ret = XAllocNamedColor(display,
						   DefaultColormap(display,DefaultScreen(display)),
						   "darkgoldenrod2",
						   &dummy, &csr_warn);

	cursor = XCreatePixmapCursor(display,csr_source,csr_mask,&csr_fg,&csr_bg,
								 lock_x_hot,lock_y_hot);

	XMapWindow(display,window);
	if (XGrabKeyboard(display,window,False,GrabModeAsync,GrabModeAsync,
					  CurrentTime)!=GrabSuccess) {
		exit(1);
	}
	if (XGrabPointer(display,window,False,(KeyPressMask|KeyReleaseMask)&0,
					 GrabModeAsync,GrabModeAsync,None,
					 cursor, CurrentTime)!=GrabSuccess) {
		XUngrabKeyboard(display,CurrentTime);
		exit(1);
	}

	cursor_err = XCreatePixmapCursor(display, csr_source, csr_mask, &csr_fg, &csr_err,
									 lock_x_hot, lock_y_hot);
	cursor_warn = XCreatePixmapCursor(display, csr_source, csr_mask, &csr_fg, &csr_warn,
									  lock_x_hot, lock_y_hot);

	Cursor*		curr_cursor = &cursor;
	Cursor*		last_cursor = &cursor;

	for (;;) {
		XNextEvent(display,&ev);
		switch (ev.type)
		{
		case KeyRelease:
            if (ev.xkey.state & LockMask)
                printf("lock on\n");
            else
                printf("lock off\n");
            break;

		case KeyPress:
			if (ev.xkey.time < timeout) { XBell(display,0); break; }
			clen= XLookupString(&ev.xkey,cbuf,9,&ks,0);

			switch (ks) {

			case XK_Escape: case XK_Clear:
				rlen=0; break;

			case XK_Delete: case XK_BackSpace:
				rlen = 0;
				break;

			case XK_Linefeed: case XK_Return:
				if (rlen==0) break;
				rbuf[rlen]=0;
				if (passwordok(rbuf)) goto loop_x;
				curr_cursor = &cursor_err;
				XBell(display,0);
				rlen = 0;
				if (timeout) {
					goodwill+= ev.xkey.time - timeout;
					if (goodwill > MAXGOODWILL) {
						goodwill= MAXGOODWILL;
					}
				}
				timeout= -goodwill*GOODWILLPORTION;
				goodwill+= timeout;
				timeout+= ev.xkey.time + TIMEOUTPERATTEMPT;
				break;

			default:
				if (clen != 1) break;
				/* allow space for the trailing \0 */
				if (rlen < (sizeof(rbuf) - 1)){
					rbuf[rlen] = cbuf[0];
					rlen++;
				}

				break;
			}

			if (curr_cursor == last_cursor)
			{
				if (rlen > 0)
					curr_cursor = &cursor_warn;
				else
					curr_cursor = &cursor;
			}
			
			if (curr_cursor != last_cursor)
			{
				XGrabPointer(display,window,False,(KeyPressMask|KeyReleaseMask)&0,
							 GrabModeAsync,GrabModeAsync,None,
							 *curr_cursor, CurrentTime);
				last_cursor = curr_cursor;
			}

			break;
		default:
			break;
		}
	}
 loop_x:
	exit(0);
}
