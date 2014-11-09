import numpy as np
import cv2

def open_image(path):
    img = cv2.imread(path, 0)
    return img

def open_colored(path):
    img = cv2.imread(path, cv2.CV_LOAD_IMAGE_COLOR)
    return img

def show_image(name, img):
    cv2.imshow(name, img)
    cv2.moveWindow(name, 500, 200)

def write_image(name, img):
    cv2.imwrite(name, img)
    show_image(name, img)

def wait_exit():
    cv2.waitKey(0)
    cv2.destroyAllWindows()

def highlight_text(img):
    cpy = img.copy()
    cpy = cv2.GaussianBlur(cpy, (3, 3), 0)
    cpy = cv2.Laplacian(cpy, 0, cpy, 3, 1)
    cv2.threshold(cpy, 5, 255, cv2.THRESH_BINARY, cpy)
    return cpy

def contours(img):
    return cv2.findContours(img.copy(), cv2.RETR_LIST, cv2.CHAIN_APPROX_NONE)[0]

def border_contours(img, contours, color):
    for contour in contours:
        x, y, w, h = cv2.boundingRect(contour)
        cv2.rectangle(img, (x, y), (x + w, y + h), color)
    return img

def words(img):
    krl = np.ones((3, 5), np.uint8)
    img = cv2.morphologyEx(img, cv2.MORPH_CLOSE, krl)
    (h, w) = img.shape
    mask = np.zeros((h + 2, w + 2, 1), np.uint8)
    for c in contours(img):
        x, y, w, h = cv2.boundingRect(c)
        cv2.floodFill(img, mask, (x + w/2, y + h/2), 100)
    cv2.threshold(img, 50, 255, cv2.THRESH_BINARY, img)
    return contours(img)

def main():
    name = 'text.bmp'
    img = open_image(name)
    txt = highlight_text(img)
    word_contours = words(txt)
    res = border_contours(open_colored(name), word_contours, (50, 150, 50))
    write_image("out.png", res)
    wait_exit()

if __name__ == '__main__':
    main()


