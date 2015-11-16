{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}
module Lib.Image
    ( convert
    ) where

import Codec.Picture
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB

convert :: (Maybe Int, Maybe Int) -> B.ByteString -> Either String LB.ByteString
convert (outWidth, outHeight) bs = do
  input <- decodeImage bs
  case input of
    ImageYCbCr8 img@(Image w h _) -> return $ encodeJpeg $ resize (resizeTo w h outWidth outHeight) img
    _ -> Left "Unsupported format"
  where
    resizeTo origW origH Nothing Nothing = (origW, origH)
    resizeTo origW origH (Just w) Nothing = (w, origH * w `quot` origW)
    resizeTo origW origH Nothing (Just h) = (origW * h `quot` origH, h)
    resizeTo origW origH (Just w) (Just h) = (w, h)

-- http://qiita.com/fumieval/items/2c761afb18e65c1fad06

bilinear :: (Integral (PixelBaseComponent a), Pixel a) => a -> a -> a -> a -> Float -> Float -> a
bilinear p q r s u v = mixWith (f v) (mixWith (f u) p q) (mixWith (f u) r s) where
        f t _ x y = floor $ fromIntegral x * (1 - t) + fromIntegral y * t

resize ::  (Integral (PixelBaseComponent a), Pixel a) => (Int, Int) -> Image a -> Image a
resize (w, h) img@(Image w0 h0 _) = generateImage f w h where
    f x y = let x' = fromIntegral x / fromIntegral w * fromIntegral w0
                y' = fromIntegral y / fromIntegral h * fromIntegral h0
                x0 = floor x'
                y0 = floor y'
                x1 = min (x0 + 1) x
                y1 = min (y0 + 1) y
            in bilinear
                (pixelAt img x0 y0)
                (pixelAt img x1 y0)
                (pixelAt img x0 y1)
                (pixelAt img x1 y1)
                    (x' - fromIntegral x0)
                    (y' - fromIntegral y0)

