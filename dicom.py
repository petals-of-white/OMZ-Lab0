import pydicom as dicom
import matplotlib.pyplot as plt

# specify your image path
image_path = "C:/Users/maxle/Обробка_медичних_зображень/Lab0/DICOM_Image_8b.dcm"
ds = dicom.dcmread(image_path)

plt.imshow(ds.pixel_array)