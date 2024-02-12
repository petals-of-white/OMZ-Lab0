-----------------------------------------------------------------------------
--
-- Module      :  Data.DICOM.Tag
-- Copyright   :  Copyright (c) DICOM Grid 2015
-- License     :  GPL-3
--
-- Maintainer  :  paf31@cantab.net
-- Stability   :  experimental
-- Portability :
--
-----------------------------------------------------------------------------
{-# LANGUAGE PatternSynonyms #-}

module Data.DICOM.Tag
  ( TagGroup (TagGroup, runTagGroup)
  , TagElement (TagElement, runTagElement)

  , Tag (tagGroup, tagElement)

  , pattern SequenceGroup

  , pattern Item
  , pattern ItemDelimitationItem
  , pattern SequenceDelimitationItem
  , pattern PixelData
  , pattern Rows
  , pattern Columns
  , pattern RescaleIntercept
  , pattern RescaleSlope
  , pattern SamplesPerPixel
  , pattern PhotometricInterpretation
  , pattern BitsAllocated
  , tag
  ) where

import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Word

import           Control.Applicative

newtype TagGroup = TagGroup { runTagGroup :: Word16 } deriving (Show, Eq, Ord)

newtype TagElement = TagElement { runTagElement :: Word16 } deriving (Show, Eq, Ord)

data Tag = Tag
  { tagGroup   :: TagGroup
  , tagElement :: TagElement
  } deriving (Show, Eq, Ord)

-- Serialization

instance Binary TagGroup where
  get = TagGroup <$> getWord16le
  put = putWord16le . runTagGroup

instance Binary TagElement where
  get = TagElement <$> getWord16le
  put = putWord16le . runTagElement

instance Binary Tag where
  get = Tag <$> get <*> get
  put t = do
    put (tagGroup t)
    put (tagElement t)


-- Smart constructors

tag :: TagGroup -> TagElement -> Tag
tag = Tag


-- tags
pattern Rows = Tag (TagGroup 0x0028) (TagElement 0x0010)
pattern Columns = Tag (TagGroup 0x0028) (TagElement 0x0011)
pattern BitsAllocated = Tag (TagGroup 0x0028) (TagElement 0x0100)
pattern BitsStored = Tag (TagGroup 0x0028) (TagElement 0x0101)
pattern HighBit = Tag (TagGroup 0x0028) (TagElement 0x0102)
pattern SamplesPerPixel = Tag (TagGroup 0x0028) (TagElement 0x0002)
pattern PhotometricInterpretation = Tag (TagGroup 0x0028) (TagElement 0x0004)
pattern RescaleIntercept = Tag (TagGroup 0x0028) (TagElement 0x1052)
pattern RescaleSlope = Tag (TagGroup 0x0028) (TagElement 0x1053)

-- Special tags
pattern SequenceGroup :: TagGroup
pattern SequenceGroup                = TagGroup 0xFFFE

pattern Item                         = Tag SequenceGroup (TagElement 0xE000)
pattern ItemDelimitationItem         = Tag SequenceGroup (TagElement 0xE00D)
pattern SequenceDelimitationItem     = Tag SequenceGroup (TagElement 0xE0DD)

pattern PixelData                    = Tag (TagGroup 0x7FE0) (TagElement 0x0010)


