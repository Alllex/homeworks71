import numpy as np
import cv2

def open_image(path):
    img = cv2.imread(path, 0)
    return img

SCR_WIDTH = 1368
sum_height = 20
max_height = 0
rightmost = 50
def show_image(name, img):
    global sum_height, rightmost, max_height
    cv2.imshow(name, img)
    x, y = 0, 0
    h, w = img.shape
    h += 30
    max_height = max(max_height, h)
    if w + rightmost > SCR_WIDTH: 
        rightmost = 50 + w
        sum_height += max_height
        max_height = 0
        x, y = 0, sum_height
    else:
        x, y = rightmost, sum_height
        rightmost += w
    cv2.moveWindow(name, x, y)
    # cv2.imwrite("output.png", img)
    print 'Window(%s) Img(%s, %s) Pos(%s, %s)' % (name, w, h, x, y)


def wait_exit():
    cv2.waitKey(0)
    cv2.destroyAllWindows()

def blur(src, w, h, x, y=0):
    if w%2==0: w+=1
    if h%2==0: h+=1
    cpy = src.copy()
    cv2.GaussianBlur(cpy, (w,h), x, cpy, y)
    return cpy

def lapl(src, size):
    if size%2==0: size+=1
    cpy = src.copy()
    cv2.Laplacian(cpy, 0, cpy, size, 1)
    return cpy

def thre(src, bias):
    cpy = src.copy()
    cv2.threshold(src, bias, 255, cv2.THRESH_BINARY, cpy)
    return cpy


def main():
    src = open_image('src/text.bmp')
    # e1 = cv2.getTickCount()
    # e2 = cv2.getTickCount()
    # time = (e2 - e1) / cv2.getTickFrequency()

    show_image("source", src)
    # size = 3
    # for x in range(0, 30, 5):
    #     w, h, y = 10 * size, size, .1
    #     show_image("(w = %s, h = %s), (x = %s, y = %s)" % (w, h, x, h), blur(src, w, h, x, y))

    img = src.copy()
    for x in range(5, 30, 5):
        ll = lapl(img, x)
        bl = blur(ll, 20, 3, 50, .1)
        show_image("#1 - %s" % (x,), bl)

    img = src.copy()
    img = blur(img, 20, 3, 50, .1)
    show_image("img", img)
    for x in range(5, 30, 5):
        ll = lapl(img, x)
        bl = blur(ll, 3, 7, .1, 4)
        show_image("#2 - %s" % (x,), bl)

    img = src.copy()
    img = blur(img, 20, 3, 50, .1)
    show_image("img2", img)
    for x in range(5, 30, 5):
        ll = lapl(img, x)
        bl = blur(ll, 3, 7, .1, 4)
        th = thre(bl, 100)
        bl = blur(th, 20, 3, 50, .1)
        show_image("#3 - %s" % (x,), bl)

    # print "time elapsed: ", time
    wait_exit()


if __name__ == '__main__':
    main()