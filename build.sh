name=$(basename "$(pwd)")
build_number=${1?supply a build number - see *.build-number.current}

lispworks -build main.build

xattr -c "icon.icns"
cp "icon.icns" "$name.app/Contents/Resources/app.icns"

sed 's|7.0.0|'$build_number'|g;s|</dict>|\
<key>LSApplicationCategoryType</key>\
<string\>public.app-category.games</string>\
</dict>|' "$name.app/Contents/Info.plist" > "$name.app/Contents/Info.plist,2"
mv "$name.app/Contents/Info.plist,2" "$name.app/Contents/Info.plist"
	
codesign -v --deep --entitlements sandbox.entitlements -s "3rd Party Mac Developer Application: xxxxxxxxxxxxxxxxx" "$name.app/" # 

productbuild --component "$name.app" /Applications --sign "3rd Party Mac Developer Installer: xxxxxxxxxxxxxxxxx"$name.app"/Contents/Info.plist "$name".pkg


