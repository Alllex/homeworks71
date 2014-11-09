import numpy as np
import cv2

def open_image(path):
    img = cv2.imread(path, 0)
    return img

def highlight_text(img):
    cpy = img.copy()
    cpy = cv2.GaussianBlur(cpy, (3, 3), 0)
    cpy = cv2.Laplacian(cpy, 0, cpy, 3, 1)
    cv2.threshold(cpy, 30, 255, cv2.THRESH_BINARY, cpy)
    return cpy

def show_image(name, img):
    cv2.imshow(name, img)
    cv2.moveWindow(name, 500, 200)

def write_image(name, img):
    cv2.imwrite(name, img)
    show_image(name, img)

def wait_exit():
    cv2.waitKey(0)
    cv2.destroyAllWindows()

def main():
    img = open_image("text.bmp")
    out = highlight_text(img)
    write_image("out.png", out)
    wait_exit()

if __name__ == '__main__':
    main()


