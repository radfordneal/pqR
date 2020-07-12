/*
 * GraphApp - Cross-Platform Graphics Programming Library.
 *
 * File: bitmaps.c -- functions for creating a detroying bitmaps.
 * Platform: Windows  Version: 2.40  Date: 1998/05/05
 *
 * Version: 1.00  Changes: Original version by Lachlan Patrick.
 * Version: 2.00  Changes: New object class system implemented.
 * Version: 2.15  Changes: Improved bitmap data formats.
 * Version: 2.30  Changes: Now uses bitmap_base.
 * Version: 2.40  Changes: Fast image to bitmap conversion.
 */

/* Copyright (C) 1993-1998 Lachlan Patrick

   This file is part of GraphApp, a cross-platform C graphics library.

   GraphApp is free software; you can redistribute it and/or modify it
   under the terms of the GNU Library General Public License.
   GraphApp is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY.

   See the file COPYLIB.TXT for details.
*/

#include "internal.h"

/*
 *  Internal bitmap deletion function.
 */
static void private_delbitmap(bitmap obj)
{
    if (obj->handle)
	DeleteObject((HBITMAP) obj->handle);
}

/*
 *  Create/return the base bitmap object.
 */
static object get_bitmap_base(void)
{
    static object bitmap_base = NULL;

    if (! bitmap_base)
	bitmap_base = new_object(BaseObject, 0, NULL);
    return bitmap_base;
}

/*
 *  Create a white bitmap.
 */
bitmap newbitmap(int width, int height, int depth)
{
    BITMAP bm;
    HBITMAP hb, old;
    HDC dc, screendc;

    screendc = GetDC(0); /* get screen dc */

    if (depth == 1) /* monochrome bitmap required */
    {
	hb = CreateBitmap(width, height, 1, 1, 0);
    }
    else /* colour bitmap required */
    {
	hb = CreateCompatibleBitmap(screendc, width, height);
    }

    if (! hb) {
	ReleaseDC(0, screendc);
	return NULL;
    }

    dc = CreateCompatibleDC(screendc);
    old = SelectObject(dc, hb);
    PatBlt(dc, 0, 0, width, height, WHITENESS);
    SelectObject(dc, old);
    DeleteDC(dc);

    ReleaseDC(0, screendc);

    GetObject(hb, sizeof(BITMAP), (LPSTR) &bm);

    bitmap obj = new_object(BitmapObject, hb, get_bitmap_base());
    if (! obj) {
	DeleteObject(hb);
	return NULL;
    }
    obj->rect = rect(0,0,width,height);
    obj->depth = (bm.bmPlanes * bm.bmBitsPixel);
    obj->die = private_delbitmap;

    return obj;
}

/*
 *  Check an image to see if there are any transparent pixels.
 *  Return 1 as soon as a transparent pixel is found, 0 if none are.
 */
PROTECTED
int has_transparent_pixels(image img)
{
    int i, width, height, total;
    rgb *palette;
    byte *pixel8;
    rgb *pixel32;
    rgb col;

    if (! img)
	return 0;

    width = getwidth(img);
    height = getheight(img);
    total = width * height;

    palette = getpalette(img);

    pixel8 = getpixels(img);
    pixel32 = (rgb *) pixel8;

    if (getdepth(img) == 8) {
	for (i=0; i < total; i++) {
	    col = pixel8[i];
	    col = palette[col];
	    if (getalpha(col) > 0x7F)
		return 1;
	}
    }
    else {
	for (i=0; i < total; i++) {
	    col = pixel32[i];
	    if (getalpha(col) > 0x7F)
		return 1;
	}
    }

    return 0;
}

/*
 *  Create a bitmap version of an image.
 */
// See also http://msdn.microsoft.com/en-us/library/dd183353%28v=VS.85%29.aspx
bitmap imagetobitmap(image img)
{
    unsigned row_bytes;
    unsigned size;

    if (!img) return NULL;

    /* remember some facts about the image */
    int width = getwidth(img), height = getheight(img);
    int depth = getdepth(img);
    rgb *palette = getpalette(img);
    int palsize = getpalettesize(img);
    byte *pixel8 = getpixels(img);
    rgb *pixel32 = (rgb *) pixel8;

    /* create DIB info in memory */
    size = sizeof(BITMAPINFOHEADER) + 4; /* Why + 4 ? */
    size += (sizeof(RGBQUAD) * palsize);
    /* rows need to be whole (4-byte) words */
    if (depth == 8) row_bytes = ((width + 3) / 4) * 4;
    else if (depth == 24) row_bytes = (((width * 3) + 3) / 4) * 4;
    else row_bytes = 4 * width;
    size += (row_bytes * height);

    /* create the block, align on LONG boundary
       malloc will have done the alignment, which needs to 
       be for pointers.
    */
    byte *block = array(size, byte);
    BITMAPINFO *bmi = (BITMAPINFO *) block;
	

    /* assign header info */
    bmi->bmiHeader.biSize = sizeof(BITMAPINFOHEADER);
    bmi->bmiHeader.biWidth = width;
    bmi->bmiHeader.biHeight = -height;  // use negative value to flip
    bmi->bmiHeader.biPlanes = 1;
    bmi->bmiHeader.biBitCount = depth;
    bmi->bmiHeader.biCompression = BI_RGB;
    bmi->bmiHeader.biClrUsed = palsize;

    /* assign colour table */
    for (int i = 0; i < palsize; i++) {
	rgb colour  = palette[i];
	byte *data = (byte *) (& bmi->bmiColors[i]);
	data[0] = getblue(colour);
	data[1] = getgreen(colour);
	data[2] = getred(colour);
	data[3] = 0;
    }

    /* assign the bitmap data itself */
    byte *data = (byte *) (&bmi->bmiColors[palsize]);

    if (depth == 8)
	for (unsigned y = 0; y < height; y++) {
	    // memcpy(data + y * row_bytes, pixel8 + y * width, width);
	    for (unsigned x = 0; x < width; x++)
		data[y * row_bytes + x] = pixel8[y * width + x];
	}
    else if (depth == 24)
	for (unsigned y = 0; y < height; y++) {
	    for (unsigned x = 0; x < width; x++) {
		rgb colour = pixel32[y * width + x];
		int i = y * row_bytes + 3*x;
		data[i]   = getblue(colour);
		data[i+1] = getgreen(colour);
		data[i+2] = getred(colour);
	    }
	}
    else memcpy(data, pixel32, 4*height*width);

    /* Create the bitmap from the DIB data.
       Could also use CreateDIBsection */

    HDC screendc = GetDC(0);
    HBITMAP hb = CreateDIBitmap(screendc, &bmi->bmiHeader, CBM_INIT,
				data, bmi, DIB_RGB_COLORS);
    ReleaseDC(0, screendc);

    /* tidy up */
    discard(block);

    /* create the bitmap */
    bitmap obj = new_object(BitmapObject, hb, get_bitmap_base());
    if (!obj) {
	DeleteObject(hb);
	return NULL;
    }
    obj->rect = rect(0, 0, width, height);
    obj->depth = depth;
    obj->die = private_delbitmap;

    return obj;
}

#ifdef UNUSED
/*
 *  The loadicon function returns a HBITMAP of an ICON from resources.
 */
static bitmap loadicon(const char *name)
{
    HICON i;
    HDC dc;
    HBITMAP old;
    bitmap obj;

    if (this_instance == 0)
	return NULL;
    if ((i = LoadIcon(this_instance, name)) == 0)
	return NULL;
    if ((obj = newbitmap(32, 32, 0)) == NULL)
	return NULL;

    dc = CreateCompatibleDC(0);
    old = SelectObject(dc, (HBITMAP)obj->handle);
    DrawIcon(dc, 0, 0, i);
    SelectObject(dc, old);
    DeleteDC(dc);
    DestroyIcon(i);

    return obj;
}

/*
 *  The loadpict function returns BITMAP resources.
 */
static bitmap loadpict(const char *name)
{
    BITMAP bm;
    HBITMAP hb;
    bitmap obj;

    if (this_instance == 0)
	return NULL;
    hb = LoadBitmap(this_instance, name);
    GetObject(hb, sizeof(BITMAP), (LPSTR) &bm);

    obj = new_object(BitmapObject, hb, get_bitmap_base());
    if (obj) {
	obj->rect = rect(0, 0, bm.bmWidth, bm.bmHeight);
	obj->depth = (bm.bmPlanes * bm.bmBitsPixel);
	obj->die = private_delbitmap;
    }
    return obj;
}

/*
 *  The loadbitmap function finds and returns bitmaps from resources,
 *  or from an image file.
 */
bitmap loadbitmap(const char *name)
{
    bitmap obj;

    if ((obj = loadpict(name)) != NULL)
	return obj;
    if ((obj = loadicon(name)) != NULL)
	return obj;
    if ((obj = imagetobitmap(loadimage(name))) != NULL)
	return obj;
    return NULL;
}

/*
 *  The following functions are obsolete from version 2.4.
 */

void setbitmapdata(bitmap obj, unsigned char *data)
{
    rect r;
    int depth, size, row_bytes;
    int x, y;
    unsigned char *newdata = NULL;

    r = obj->rect;
    depth = obj->depth;

    /* Calculate source row bytes. */
    row_bytes = ((depth * r.width) + 7) / 8;
    /* Each row must be a multiple of 16 bits wide. */
    if (row_bytes % 2) {
	/* Odd number of bytes, must assign into new array. */
	size = (row_bytes+1) * r.height;
	newdata = array (size, byte);
	if (! newdata)
	    return;
	for (y=0; y<r.height; y++) {
	    for (x=0; x<row_bytes; x++) {
		newdata[y*(row_bytes+1)+x] =
		    data[y*row_bytes+x];
	    }
	}
	SetBitmapBits((HBITMAP)obj->handle, size,
		      (LPSTR)newdata);
	discard(newdata);
    } else {
	/* Correct format already! */
	size = row_bytes * r.height;
	SetBitmapBits((HBITMAP)obj->handle, size, (LPSTR)data);
    }
}

void getbitmapdata(bitmap obj, unsigned char *data)
{
    rect r;
    int depth, size, row_bytes;
    int x, y;
    unsigned char *newdata = NULL;

    r = obj->rect;
    depth = obj->depth;

    /* Calculate destination row bytes. */
    row_bytes = ((depth * r.width) + 7) / 8;
    /* Each row must be a multiple of 16 bits wide. */
    if (row_bytes % 2) {
	/* Odd number of bytes, must assign into new array. */
	size = (row_bytes+1) * r.height;
	newdata = array (size, byte);
	if (! newdata)
	    return;
	GetBitmapBits((HBITMAP)obj->handle, size,
		      (LPSTR)newdata);
	for (y=0; y<r.height; y++) {
	    for (x=0; x<row_bytes; x++) {
		data[y*row_bytes+x] =
		    newdata[y*(row_bytes+1)+x];
	    }
	}
	discard(newdata);
    } else {
	/* Correct format already! */
	size = row_bytes * r.height;
	GetBitmapBits((HBITMAP)obj->handle, size, (LPSTR)data);
    }
}

bitmap createbitmap(int width, int height, int depth, unsigned char *data)
{
    bitmap obj;

    if ((obj = newbitmap(width, height, depth)) == NULL)
	return NULL;
    if (data)
	setbitmapdata(obj, data);
    return obj;
}
#endif

void getbitmapdata2(bitmap obj, unsigned char **data)
{
    rect r = obj->rect;
    int depth = 32, size, ret;
    BITMAPINFO bmi;

    bmi.bmiHeader.biSize = sizeof(BITMAPINFOHEADER);
    bmi.bmiHeader.biWidth = r.width;
    bmi.bmiHeader.biHeight = -r.height;
    bmi.bmiHeader.biPlanes = 1;
    bmi.bmiHeader.biBitCount = depth;
    bmi.bmiHeader.biCompression = BI_RGB;
    bmi.bmiHeader.biClrUsed = 0;

    size = 4 * r.width * r.height;
    *data = (unsigned char *) malloc(size);
    if(*data) {
	ret = GetDIBits(get_context(obj), (HBITMAP)obj->handle, 
			0, r.height, (LPSTR)*data, &bmi, DIB_RGB_COLORS);
	if(!ret) {
	    free(*data);
	    *data = NULL;
	}
    }
}

