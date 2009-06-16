
# You will have to customize these
uuid=40AB56A9-5BAC-42E6-A550-8A25484BCF49
#gsc=/usr/local/Gambit-C/iPhoneSimulator/bin/gsc
gsc=/usr/local/Gambit-C/iPhoneOS/bin/gsc
root=`pwd`

# You can customize these, but you shouldn't have to
app_name=tosser.app
exe_name=tosser
deploy_path=~/Library/'Application Support/iPhone Simulator'/User/Applications/$(uuid)
gcc=/Developer/Platforms/iPhoneOS.platform/Developer/usr/bin/arm-apple-darwin9-gcc-4.0.1
sdk=/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS2.0.sdk

# gcc=/Developer/Platforms/iPhoneSimulator.platform/Developer/usr/bin/gcc-4.0
# sdk=/Developer/Platforms/iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator2.0.sdk


all: tosser.app lib/graphics.o1 lib/ffi/gl.o1 lib/util/srfi-1.o1

lib/init_.c: lib/init.scm
	cd lib && $(gsc) -debug -link init.scm

lib/ffi/gl.o1: lib/ffi/gl.scm
	rm -rf lib/ffi/gl.o1
	cd lib/ffi && $(gsc) -debug gl.scm

lib/util/srfi-1.o1: lib/util/srfi-1.scm
	rm -rf lib/util/srfi-1.o1
	cd lib/util && $(gsc) -debug srfi-1.scm

lib/graphics.o1: lib/graphics.scm
	rm -rf lib/graphics.o1
	cd lib && $(gsc) graphics.scm

tosser.app: Info.plist app/main.m app/EAGLView.m lib/init_.c app/cocoa-ffi.m
	echo '(define root "$(root)")' > lib/config.scm

	mkdir -p $(app_name)
	cp Info.plist $(app_name)
	ibtool --errors --warnings --notices \
		--output-format human-readable-text \
		--compile $(app_name)/window.nib \
		app/window.xib

	$(gcc) -x objective-c -isysroot $(sdk) \
	-framework Foundation -framework UIKit \
	-framework OpenGLES -framework QuartzCore \
	-fvisibility=hidden -I/usr/local/include -D___LIBRARY -lgambc \
    -I/usr/local/Gambit-C/iPhoneOS/include \
    -L/usr/local/Gambit-C/iPhoneOS/lib \
	app/EAGLView.m app/tosserAppDelegate.m app/main.m \
    app/cocoa-ffi.m lib/init*.c \
	-o $(app_name)/$(exe_name)

# 	rm -fr $(deploy_path)/$(app_name)
# 	cp -r $(app_name) $(deploy_path)
