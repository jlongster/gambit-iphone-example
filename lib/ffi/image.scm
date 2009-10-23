
(c-declare #<<end-c-code

CGImageRef CGImageRef_loadpng(const char *filename) {
    CFBundleRef mainBundle = CFBundleGetMainBundle();

    CFStringRef name = CFStringCreateWithCString(NULL, filename, kCFStringEncodingUTF8);
    CFURLRef url = CFBundleCopyResourceURL(mainBundle, name, CFSTR("png"), NULL);
    CFRelease(name);

    if(!url) {
        return NULL;
    }
    
    CGDataProviderRef provider = CGDataProviderCreateWithURL(url);
    CFRelease(url);

    CGImageRef image = CGImageCreateWithPNGDataProvider(provider, NULL, true,
                                                        kCGRenderingIntentDefault);
    CGDataProviderRelease(provider);
    
    return image;
}

unsigned char* CGImageRef_data(CGImageRef image) {
    NSInteger width = CGImageGetWidth(image);
    NSInteger height = CGImageGetHeight(image);
    unsigned char *data = (unsigned char*)malloc(width*height*4);

    NSLog(@"%x %d %d", image, width, height);
    
    CGContextRef context = CGBitmapContextCreate(data,
                                                 width, height,
                                                 8, width * 4,
                                                 CGImageGetColorSpace(image),
                                                 kCGImageAlphaPremultipliedLast);

    CGContextDrawImage(context,
                       CGRectMake(0.0, 0.0, (float)width, (float)height),
                       image);
	CGContextRelease(context);

    return data;
}

end-c-code
)

(define CGImageRef-width
  (c-lambda (CGImageRef) int "___result = CGImageGetWidth(___arg1);"))

(define CGImageRef-height
  (c-lambda (CGImageRef) int "___result = CGImageGetHeight(___arg1);"))

(define CGImageRef-loadpng
  (c-lambda (char-string) CGImageRef "CGImageRef_loadpng"))

(define CGImageRef-data
  (c-lambda (CGImageRef) unsigned-int8-array "CGImageRef_data"))
