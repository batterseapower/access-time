{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module System.Directory.AccessTime (
    getAccessTime, getAccessTimeResolution
  ) where

import System.Time

#ifdef UNIX

import Data.Time.Clock.POSIX

import System.Posix.Types
import System.Posix.Files

getAccessTime fp = fmap (epochTimeToClockTime . accessTime) $ getFileStatus fp

epochTimeToClockTime :: EpochTime -> ClockTime
epochTimeToClockTime x = TOD whole_secs (round fractional_picosecs)
  where (whole_secs, fractional_sec) = properFraction (toRational x)
        fractional_picosecs = 1000000000000 * fractional_sec

-- See <http://en.wikipedia.org/wiki/Stat_(Unix)>: time_t provides times accurate to 1 second
getAccessTimeResolution _ = return $ noTimeDiff { tdSec = 1 }

#elif defined(WINDOWS)

import Foreign.Ptr

import System.FilePath.Windows

import System.Win32.Time
import System.Win32.Types

getAccessTime fp = do
    (_creation_time, access_time, _write_time) <- getFileTime h
    fmap systemTimeToClockTime $ fileTimeToSystemTime access_time

systemTimeToClockTime :: SYSTEMTIME -> ClockTime
systemTimeToClockTime time = toClockTime $ CalendarTime {
    ctYear = fromIntegral (wYear time),
    ctMonth = fromIntegral (wMonth time),
    ctDay = fromIntegral (wDay time),
    ctHour = fromIntegral (wHour time),
    ctMin = fromIntegral (wMinute time),
    ctSec = fromIntegral (wSecond time),
    ctPicosec = fromIntegral (wMilliseconds time) * 1000000000,
    ctWDay = error "Documented as ignored: ctWDay",
    ctYDay = error "Documented as ignored: ctYDay",
    ctTZName = error "Documented as ignored: ctTZName",
    ctTZ = 0, -- UTC has no variation from UTC!
    ctIsDST = error "Documented as ignored: ctIsDST"
  }

-- See <http://msdn.microsoft.com/en-us/library/ms724320(v=vs.85).aspx>: FAT access time has a resolution of 1 day,
-- NTFS access time has a resolution of one hour.
--
-- If neither of those cases seem to apply, we assume a 1 second resolution
getAccessTimeResolution fp = do
    fs <- getVolumeFileSystem (takeDrive fp)
    return $ case fs of "NTFS"        -> noTimeDiff { tdHour = 1 }
                        'F':'A':'T':_ -> noTimeDiff { tdDay = 1 }
                        _             -> noTimeDiff { tdSec = 1 }

-- See http://msdn.microsoft.com/en-us/library/aa364993(v=VS.85).aspx
foreign import stdcall "Windows.h GetVolumeInformationW" c_getVolumeInformationW :: LPCTSTR -- ^ lpRootPathName: A pointer to a string that contains the root directory of the volume to be described
                                                                                 -> LPTSTR  -- ^ lpVolumeNameBuffer: A pointer to a buffer that receives the name of a specified volume
                                                                                 -> DWORD   -- ^ nVolumeNameSize: The length of a volume name buffer, in TCHARs
                                                                                 -> LPDWORD -- ^ lpVolumeSerialNumber: A pointer to a variable that receives the volume serial number
                                                                                 -> LPDWORD -- ^ lpMaximumComponentLength: A pointer to a variable that receives the maximum length, in TCHARs, of a file name component that a specified file system supports
                                                                                 -> LPDWORD -- ^ lpFileSystemFlags: A pointer to a variable that receives flags associated with the specified file system
                                                                                 -> LPTSTR  -- ^ lpFileSystemNameBuffer: A pointer to a buffer that receives the name of the file system, for example, the FAT file system or the NTFS file system
                                                                                 -> DWORD   -- ^ nFileSystemNameSize: The length of the file system name buffer, in TCHARs
                                                                                 -> IO BOOL -- ^ If all the requested information is retrieved, the return value is nonzero. To get extended error information, call GetLastError.

getVolumeFileSystem :: FilePath -> IO String
getVolumeFileSystem fp = withTString fp $ \fp_tstr -> withTString (replicate ' ' fs_len) $ \fs_tstr -> do
    failIfFalse_ "GetVolumeInformation" $ c_getVolumeInformationW fp_tstr nullPtr 0 nullPtr nullPtr nullPtr fs_tstr fs_len
    peekTString fs_tstr
  where
    -- The documentation states that the file system name will never exceed this length
    fs_len = mAX_PATH + 1
    mAX_PATH = 260 -- See <http://msdn.microsoft.com/en-us/library/aa365247(v=vs.85).aspx>

#else

#error Unsupported platform

#endif


-- | The 'getAccessTime' operation returns the clock time at which the file or directory was last accessed.
--
-- The operation may fail with:
-- 
-- * 'System.IO.Error.isPermissionError' if the user is not permitted to obtain the access time; or
--
-- * 'System.IO.Error.isDoesNotExistError' if the file or directory does not exist.
getAccessTime :: FilePath -> IO ClockTime

-- | Approximate resolution of access times on your system for the given file or directory.
-- Presently this will not attempt to determine whether access times are enabled on the relevant file system.
getAccessTimeResolution :: FilePath -> IO TimeDiff
