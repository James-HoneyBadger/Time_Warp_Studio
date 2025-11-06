This folder is a template for an `.iconset` used by `iconutil` to make an `.icns` file.

Create PNGs with these names and sizes (Apple expects 1x and 2x variants):

- icon_16x16.png
- <icon_16x16@2x.png> (32x32)
- icon_32x32.png
- <icon_32x32@2x.png> (64x64)
- icon_128x128.png
- <icon_128x128@2x.png> (256x256)
- icon_256x256.png
- <icon_256x256@2x.png> (512x512)
- icon_512x512.png
- <icon_512x512@2x.png> (1024x1024)

Then run the helper to produce an `.icns` file:

```bash
./scripts/make_icns.sh packaging/macos/icon.iconset packaging/macos/resources/app.icns
```

Place the resulting `app.icns` under `packaging/macos/resources/` so the packaging script will copy it into the `.app` bundle.
