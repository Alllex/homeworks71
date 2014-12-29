from cv2 import *
import numpy as np

def trackPoints(vid, name, detector):
    optflow_params = dict( 
        nextPts  = None,
        winSize  = (15, 15),
        maxLevel = 2,
        criteria = (TERM_CRITERIA_EPS | TERM_CRITERIA_COUNT, 10, 0.03)
    )
    color = np.random.randint(0, 255, (100,3))
    ret, old = vid.read()
    prev = cvtColor(old, COLOR_BGR2GRAY)
    fourcc = cv.CV_FOURCC(*'DIVX')
    output = VideoWriter(name, fourcc, 20.0, prev.shape)
    prevPts = detector(prev)
    mask = np.zeros_like(old)

    while(True):
        ret, img = vid.read()
        if not ret: break
        next = cvtColor(img, COLOR_BGR2GRAY)
        nextPts, st, _ = calcOpticalFlowPyrLK(prev, next, prevPts, **optflow_params)
        nextGd = nextPts[st == 1]

        for i, (new, old) in enumerate(zip(nextGd, prevPts[st == 1])):
            x1, y1 = new.ravel()
            x2, y2 = old.ravel()
            clr = color[i % 100].tolist()
            line(mask, (x1, y1), (x2, y2), clr, 2)
            circle(img, (x1, y1), 3, clr, -1)

        img = add(img, mask)
        output.write(img)
        imshow('Sample', img)

        if 27 == waitKey(30) & 0xff: break
        prev = next.copy()
        prevPts = nextGd.reshape(-1, 1, 2)

    return output

def harrisDetector(frame):
    return goodFeaturesToTrack(
            image = frame, 
            mask = None, 
            maxCorners = 100,
            qualityLevel = 0.1,
            minDistance = 10,
            blockSize = 11,
            useHarrisDetector = True
    )

def fastDetector(frame):
    Fast = FastFeatureDetector(150, FAST_FEATURE_DETECTOR_TYPE_9_16)
    toArr = [np.float32( np.array([np.array(x.pt)])) for x in Fast.detect(frame, None)]
    return np.array(toArr)

def performTrack(src, out, detector):
    video = VideoCapture(src)
    vid = trackPoints(video, out, detector)
    vid.release()

def main():
    performTrack('sequence.mpg', 'harris.avi', harrisDetector)
    performTrack('sequence.mpg', 'fast.avi',   fastDetector)
    waitKey(0)

if __name__ == '__main__':
    main()