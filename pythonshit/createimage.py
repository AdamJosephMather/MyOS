from PIL import Image

def analyze_webp_pixels(image_path):
	"""
	Loads a WebP image and iterates over its pixels, printing the color
	and coordinates (x, y).

	:param image_path: The file path to the WebP image.
	"""
	try:
		# 1. Load the WebP image
		img = Image.open(image_path)
		
		# Ensure the image is in RGB or RGBA mode for consistent color access
		# 'RGBA' is safer as it handles transparent WebP images
		img = img.convert("RGBA") 

		# 2. Get the width (w) and height (h)
		# img.size returns a tuple (width, height)
		width, height = img.size
		print(f"Image loaded: {image_path}")
		print(f"Dimensions (w, h): {width}, {height}")
		print("-" * 30)

		# 3. Iterate over the pixels (x and y)
		
		values = []
		curvalue = []
		
		for y in range(height):
			for x in range(width):
				# 4. Get the color (R, G, B, A) at the pixel (x, y)
				# getpixel((x, y)) returns a tuple of color values
				color = img.getpixel((x, y))
				
				if (color[1] < 155) :
					v = "0"
				else:
					v = "1"
				
				curvalue.append(v)
				if len(curvalue) == 64:
					binary_string = "".join(curvalue)
					decimal_number = int(binary_string, 2)
					values.append(str(decimal_number))
					curvalue = []
		
		if len(curvalue) != 0:
			curvalue += [0]*(64-len(curvalue))
			binary_string = "".join(curvalue)
			decimal_number = int(binary_string, 2)
			values.append(str(decimal_number))
			curvalue = []
		
		print("uint64_t img[] = {")
		print("\t"+(",".join(values)))
		print("}")

	except FileNotFoundError:
		print(f"Error: The file at path '{image_path}' was not found.")
	except Exception as e:
		print(f"An error occurred: {e}")

analyze_webp_pixels('small.jpg')