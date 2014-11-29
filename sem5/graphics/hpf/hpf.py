import numpy as np
import cv2

def open_image(path):
    img = cv2.imread(path, 0)
    return img

def show_image(name, img):
    cv2.imshow(name, img)

def write_image(name, img):
    cv2.imwrite(name, img)
    show_image(name, img)

def wait_exit():
    cv2.waitKey(0)
    cv2.destroyAllWindows()

# ------------------------------------------------------------------

def magnitude(img):
    cpy = img.copy()
    fshift = np.fft.fftshift(np.fft.fft2(cpy))
    mspec = 16 * np.log(np.abs(fshift))
    return (fshift, mspec.astype(np.uint8))

def shift_back(fshift, r, c):
    d = 42
    fshift[r-d:r+d, c-d:c+d] = 0
    ishift = np.fft.ifftshift(fshift)
    ifft = np.abs(np.fft.ifft2(ishift))
    return ifft.astype(np.uint8)

def lapl(img):
    cpy = img.copy()
    return cv2.Laplacian(cpy, 0, cpy, 3, 3)

def main():
    img = open_image("mandril.bmp")

    (fshift, mspec) = magnitude(img)
    
    rows, cols = img.shape
    filtered = shift_back(fshift, rows/2 , cols/2)
    
    lapled = lapl(img) 

    show_image('Mandril', img)
    show_image('Magnitude', mspec)
    write_image('hpf.png', filtered)
    write_image('lapl.png', lapled)
    
    wait_exit()

# -----------------------------------------------------------------

if __name__ == '__main__':
    main()
