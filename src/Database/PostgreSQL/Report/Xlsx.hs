module Database.PostgreSQL.Report.Xlsx (
	oneRow
	) where

import qualified Data.Map as M
import qualified Data.Text as T

import Database.PostgreSQL.Report
import Database.PostgreSQL.Sync

import Data.Conduit
import qualified Data.Conduit.List as CL

import qualified Codec.Xlsx as Xlsx
import qualified Codec.Xlsx.Parser as Xlsx
import qualified Codec.Xlsx.Writer as Xlsx

oneRow :: FilePath -> IO (Maybe (M.Map T.Text T.Text))
oneRow f = do
	x <- Xlsx.xlsx f
	runResourceT $
		Xlsx.sheetRowSource x 0 $$
		CL.peek

generateReport :: Syncs -> M.Map T.Text T.Text -> TIO [[FieldValue]]
generateReport ss m = generate (reportc ss m') where
	m' = map T.unpack $ M.elems m

saveReport :: FilePath -> [[FieldValue]] -> IO ()
saveReport f fs = Xlsx.writeXlsx f [sheet] where
	sheet = Xlsx.Worksheet {
		Xlsx.wsName = T.pack "report",
		Xlsx.wsMinX = undefined,
		Xlsx.wsMaxX = undefined,
		Xlsx.wsMinY = undefined,
		Xlsx.wsMaxY = undefined,
		Xlsx.wsColumns = undefined,
		Xlsx.wsRowHeights = undefined,
		Xlsx.wsCells = undefined }
