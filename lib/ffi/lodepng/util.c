
typedef struct __lodepng_image {
    unsigned char *data;
    unsigned int w, h;
} __lodepng_image;

___SCMOBJ __release_loadpng_image(void* obj) {
    __lodepng_image *img = (__lodepng_image*)obj;
    free(img->data);
    free(img);
    return ___FIX(___NO_ERR);
}
