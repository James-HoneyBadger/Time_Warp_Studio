from PIL import Image, ImageDraw

def create_icon():
    # Create a 256x256 image with a dark blue background
    img = Image.new('RGBA', (256, 256), color=(20, 20, 40, 255))
    d = ImageDraw.Draw(img)
    
    # Draw a "warp" spiral or circle
    for i in range(10):
        d.ellipse([20+i*10, 20+i*10, 236-i*10, 236-i*10], outline=(100+i*15, 200, 255), width=2)
    
    # Draw text "TW"
    # (Skipping text for simplicity without loading fonts, just shapes)
    d.rectangle([100, 100, 156, 156], fill=(255, 200, 50))
    
    img.save('packaging/linux/icon.png')
    print("Icon created at packaging/linux/icon.png")

if __name__ == "__main__":
    create_icon()
