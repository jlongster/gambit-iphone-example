
uuid=40AB56A9-5BAC-42E6-A550-8A25484BCF49
app_path='/Users/james/Library/Application Support/iPhone Simulator/User/Applications/$(uuid)'
app_name=tosser.app
exe_name=tosser

gsc=/usr/local/Gambit-C/iPhoneSimulator/bin/gsc

all: tosser.app lib/graphics.o1

lib/init_.c: lib/init.scm
	cd lib && $(gsc) -debug -link init.scm

lib/ffi/gl.o1: lib/ffi/gl.scm
	rm -rf lib/ffi/gl.o1
	cd lib/ffi && $(gsc) -debug gl.scm

lib/graphics.o1: lib/graphics.scm
	rm -rf lib/graphics.o1
	cd lib && $(gsc) graphics.scm

tosser.app: Info.plist main.m lib/init_.c
	mkdir -p $(app_name)
	cp Info.plist $(app_name)
	ibtool --errors --warnings --notices \
		--output-format human-readable-text \
		--compile $(app_name)/window.nib \
		window.xib

	/Developer/Platforms/iPhoneSimulator.platform/Developer/usr/bin/gcc-4.0 \
	-x objective-c -D__IPHONE_OS_VERSION_MIN_REQUIRED=20000 -isysroot \
	/Developer/Platforms/iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator2.0.sdk \
	-fvisibility=hidden -mmacosx-version-min=10.5 \
	-I/usr/local/include \
	-D___LIBRARY \
	-framework Foundation -framework UIKit -framework OpenGLES -framework QuartzCore \
	-lgambc \
	EAGLView.m tosserAppDelegate.m main.m \
	lib/init*.c \
	-o $(app_name)/$(exe_name)

	rm -fr $(app_path)/$(app_name)
	cp -r $(app_name) $(app_path)

# /Developer/Platforms/iPhoneSimulator.platform/Developer/usr/bin/gcc-4.0
# -x objective-c -arch i386 -fmessage-length=0 -pipe -std=c99
# -Wno-trigraphs -fpascal-strings -fasm-blocks -O0 -Wreturn-type
# -Wunused-variable -D__IPHONE_OS_VERSION_MIN_REQUIRED=20000 -isysroot
# /Developer/Platforms/iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator2.0.sdk
# -fvisibility=hidden -mmacosx-version-min=10.5 -gdwarf-2 -iquote
# /Users/james/projects/scheme/gambit-iphone/build/gambit-iphone.build/Debug-iphonesimulator/gambit-iphone.build/gambit-iphone-generated-files.hmap
# -I/Users/james/projects/scheme/gambit-iphone/build/gambit-iphone.build/Debug-iphonesimulator/gambit-iphone.build/gambit-iphone-own-target-headers.hmap
# -I/Users/james/projects/scheme/gambit-iphone/build/gambit-iphone.build/Debug-iphonesimulator/gambit-iphone.build/gambit-iphone-all-target-headers.hmap
# -iquote
# /Users/james/projects/scheme/gambit-iphone/build/gambit-iphone.build/Debug-iphonesimulator/gambit-iphone.build/gambit-iphone-project-headers.hmap
# -F/Users/james/projects/scheme/gambit-iphone/build/Debug-iphonesimulator
# -c main.m -o main.o
