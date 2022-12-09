module Day03 exposing (..)

import Char
import List.Extra as LX
import Set exposing (Set, fromList)


listToTuple2 : List a -> Maybe ( a, a )
listToTuple2 list =
    case list of
        [ a, b ] ->
            Just ( a, b )

        _ ->
            Nothing


parseRucksack contents =
    let
        len =
            String.length contents // 2
    in
    contents
        |> String.toList
        |> LX.groupsOf len
        |> List.map fromList
        |> listToTuple2
        |> Maybe.withDefault ( Set.empty, Set.empty )


calcValue val =
    case val of
        Just n ->
            let
                code =
                    Char.toCode n
            in
            if code >= 97 then
                code - 96

            else
                code - 64 + 26

        Nothing ->
            0

findCommon list =
    list
    |> List.map String.toList 
    |> List.map Set.fromList
    |> LX.foldl1 Set.intersect 
    |> Maybe.withDefault Set.empty 

part1 : Int
part1 =
    rawData
        |> List.map parseRucksack
        |> List.map (\( setA, setB ) -> Set.intersect setA setB)
        |> List.map Set.toList
        |> List.map List.head
        |> List.map calcValue
        |> List.sum

part2 =
    rawData
    |> LX.groupsOf 3 
    |> List.map findCommon
    |> List.map Set.toList 
    |> List.map List.head
    |> List.map calcValue
    |> List.sum 

rawData : List String
rawData =
    [ "GwrhJPDJCZFRcwfZWV"
    , "LjnQlqNpjjmpmQlLlqNfZRvQcTWcTSTTZcSQcZ"
    , "nNqjdspspngnmjmslqmjjjCDGrHPHMGddGCMCGPPPJWC"
    , "GwmVZmPWWFFmBbVbZVwmbPsTCnlgQgnQfhlffffZnlQh"
    , "DqVDSqqSMzLLDDNSHHLdqSdSllCQjsTlClhlflnTlhjgfgfM"
    , "VHJztNLHGtcbvvPG"
    , "bjrPrNCtNrjdcCPpptfpTVspDtfTtB"
    , "JGQJMJQMmmmZMnnLpLBTpHCD"
    , "WJJqWRgWlCJZhZRCQZwdPScdrPNbvzPzwvqz"
    , "QNSQNBWQNLjZBNNhLhSNRsTcsrTTVzcwZZZsfrrbwb"
    , "tCFtHpppppMldpvpqnMFmMVGrbPcrwbzswrzcccfvTfw"
    , "pdmCpgqCdmHHdJVWgSRNJDRVVj"
    , "sNrFnvNSzrjQtQjQTj"
    , "lcPmcJDLdPDbJPVLljdGGBBThBQTGwTtBw"
    , "PDLqmJmpJQfFqfqsCM"
    , "BnhctqdnqnRcBnslCJJCMrJrsG"
    , "wNDMZpbQwMpCvCGVjlss"
    , "WfzNwZFbwZzZmFZbLzNwzzzzcdqgRMTTPdHPTTPMRdcWgRPt"
    , "grsrVSFSSdFSDFVFjZZWwpWpZWZplgZZ"
    , "mcBPPPBLBfNdLlvvWljWJC"
    , "dMcmcRdbRzdVhFthSsTShM"
    , "bzvJZMTzTZSHLCCdDzmDcc"
    , "hqBqWPFssvshWvvssNqtsHftmfpHfdcdDGHmcpfctL"
    , "WvBQgNNNhghTJbJQlJTZlT"
    , "chcdwNwdbCbQctCjnnQGHsQspMHMjG"
    , "nSSSJqJZzJgWWRfZDJSnqvTTsVvvHVPpHVfpjHMTjP"
    , "BZRDRmmrDWSrZWWzWSRNhdnCFwChclFtwbNdtr"
    , "lNgmssCtqLwqCCtfsCLHPFhhhmMhVzBDbVzMDMVz"
    , "ZnRlQTlJzFQFQFVV"
    , "npZJvRRGZSnWvSvrSLglsClfpfcLgNgpHf"
    , "tVtqcVqFVtZhcfFtqlgSpmpDSDNPzSzZmNpw"
    , "LRGTHqbrHdnGHrTCSSwNDzMDwPMzNwbp"
    , "TqWGJrGHCHnTWnhsWcFthFjtfQch"
    , "qNnTbwtctvffFcqfrHjMrFjVHRjSjZDZ"
    , "dLLzWWPmCmCzGdsLgBLGGBDRMVMHRlrrrZDDZsNMrNNS"
    , "PJQWggCzWNWJzGWfchvfTbJvfnnwtf"
    , "ghzdgzzdQsdqzzhMNqQzvhgQnRRBWTjWWGTRGWwGTZhwGnBT"
    , "fsrfJHbFfDFLVLVFHrWCWrBRZZTGCCjwWZ"
    , "HLLllcDPbLPQdPspMNgvMt"
    , "fNDJqdPNbtHpCbwpCCCp"
    , "RTMRLrzGrMRMRPWnnvSmgCHFCCFmmT"
    , "WQsWQjzGWMsGQzWclQtVBJfBftNdtqVPfP"
    , "gbTCVVmDVFdsgmgrrcfwlwfTfPlcRR"
    , "qhQZqQvnQhLQhJnvfPcSwSwlfjGcqjqj"
    , "tLNZLZZJJZthpzhMZDCdFmFsmWWmtDDgsw"
    , "bqCvLvLppzPzPPvPbFztFtttBNGdGsRggSgGSHDdggHSzNgH"
    , "rMQpWfMfrcTjWJhwWHHsSBsRBdSTTNBgSR"
    , "VwfmWjwMWwccrWcWpQQFnFtlCqmltFnFLbbZmn"
    , "cWqsMWJMzqJJMHsJcqsJqTqjSbLBdfdSbtzLbbLfbSfShfhd"
    , "gplGvQmRrCrgZSZtSGZZjhbj"
    , "CQmmmmNQRPvjgRClCvmmcVHPqMFMFsWJVqFFcnTJ"
    , "QHHqvGwjjWNqvGTQGvTFcGwJRJbszcPtDbJVbtPzVbDptp"
    , "MLdrgmSgZZdhdfbLVRpszlRDstRL"
    , "gdSgMCSfdMnrghCWGRQvHwvNHjnjvv"
    , "RDBZwvZBrMlsvnlb"
    , "WdFQqdjWWcHHPrwSPnnSWnSS"
    , "mLdqgqHmcjHHjqLHjLppmhfBfgtDtBJZJfVtBZwGZB"
    , "CCWRJQnZlHtHtNZRFDcBhrcvhDrJVVDv"
    , "dPPSqLzfsqGLSTzfLzLGdLMVVgvBcmgMVwmmDFrVgmBBBr"
    , "SFjdTGzqpjdRbNRNnjtnQR"
    , "hjNcwBDDwDFcjdFfjtFhtcRsGGgTsGRRRTsGGqZGRq"
    , "gbmrLnbzLmvQJnQVVpqZTqzWSCRpqRTsSR"
    , "MQMvVMbPQQHrQMnMPldtwNNfgHtlwBhdwj"
    , "zwzwpzMfzrBMWfCCZrwzrMJDGGGnNmGNZvgNZsDDsGsG"
    , "FbFqSbcSbSHqTjmgGFnJglllsDJm"
    , "TbhVdVjqdtqTjVHqjPdthPBBWpCnRfwRPRCfBCCnWR"
    , "hlpmbfJJpCSChmJMmrSjTjcSdjTtQQTtTtjF"
    , "gqrgsqLzgnBgZGzHBnnsQNNQtjjcNNjjtNFQNcNH"
    , "LVRzgGGzzzPCVrJMbPJb"
    , "VHrmqFnVdvlzzNrr"
    , "PMtwBJPBcPwfbwBJndplLvLdLlgMMzLL"
    , "bBZnTwbtnScfQJPJwPTjqGZFsVFjDHHGhhHhVj"
    , "cftqScHJrfVfrrRZ"
    , "DTTsDvvlBbTGrWBwwsWDBbWdVpZjjZjpVPPGhRRVjVZNRPNN"
    , "lsWdWDbrTLBsbdrmdwbMJtmHMQJccFHFnJFqFt"
    , "SWNPTPVSWChCSmQQhpppJdFJLpDpgLJmLd"
    , "NGGtNtGfHtDpdJdqLB"
    , "NcsNGNjHZsZGnzZfnGhQnhPClrVlQPhTVVhl"
    , "QDdgMBsNhhMgcWbZdzmWLzFzWH"
    , "fRqRJJqGCvrJGjCRRrSJlfPtHzzPmfFbtPtLZZLnmt"
    , "VjvwwjlwVGGqJSSqJFccshpgNhQNQTsVgBgT"
    , "wvDLDwCbFgSTfTSJJgfB"
    , "qsRhmhqchmVhPdfTHJSzpCtJpfPf"
    , "hmdhrWrddmhlqCRcwQjDLMQnMFDZnlLl"
    , "trMWtlwwMplMZMCZWltDpzBLBnflVLBbHzbBSGlVlL"
    , "ghhqJTfmjQjfqqznznnHnBRzBLmn"
    , "sQhPQsjjQcQcTsPqZWwwZcFfWrWcrZww"
    , "MRVpVCZZTHWVMCHvgNvVvbQSqgQSlg"
    , "NFmnrNDDfnjFnndfssmcStvjvQQlvzvllqvwQllj"
    , "GGPNmBrFNdcfcGrsGcdmDFhJHMMhHLZJMhpLHCMMMMPJ"
    , "DSvDGdGFlGGnDZFdVSZvfPqwnfhpnrqpPNpLPrrh"
    , "sWcTjtHCsTmsCNfgMPjpfPhqhP"
    , "BtHzBzChzBBvFSDJvVzFJJ"
    , "sfsNrsFFBTfjwwtNNWHPVCVWtSCDDCDmmS"
    , "zMdhMMZnSccMmmWVWmCPlC"
    , "cLSScJZQbcvLhZvnzBwfTjrpNwNrBFffpb"
    , "TBrCBgrTngVQBVbhrCtgJJrGssGsMGRGcjMcNjfN"
    , "LZdSLvHMFdzFRWsLjcGRWWNJ"
    , "pHpzlqPqFPvdBthgMbVPDhgh"
    , "SZlnZZvBvvMrcBnllBMZSvhGMtQwFMGztthfwQtMwwPf"
    , "HLqsDgNsDLDDDjggHDHszthzFbQGTghPGQPbTfFT"
    , "dmLqDqCmFNjJsjHdssFNHDVWZccnRllnVZvRSBZrZlCc"
    , "SccnnSGGftShfHSHHhnvbMjvVlCjzbVzzbMMTbCB"
    , "gRpppNNQLWqZgPZwNWwwBMBbDlZCTzVTjHMMbBjV"
    , "dqNQPQRqrqpPcGtchhdfhHSF"
    , "mfDzgnNMMszBtJCpHlrjnFppCdHj"
    , "LLRThGGZcbClBQpdWFGl"
    , "bSqVTbBbMVMsNmNM"
    , "BTTbbLVpfchmjbsj"
    , "JSQJHDMHqdNZTZlhFFhCFFrNhNcsrr"
    , "tMwJQlwMMlQwDDJtWGLGPpWLLGnTPn"
    , "LcVQQCPPLqTzqQTcllTzhnHHfFJRcGHcFfwRGHwJjJ"
    , "stdWDDBtVgbpWgZbsNgDNdWFGMnnwHfjHFpfwwMGMMGRjJ"
    , "ZWSDtgNdWNBdgsdsNDDsdbDlTzCVSTCqQmSqTQSvhqLVQq"
    , "dZbgdZbNtmqttFJtHHzcczMcFszHnsvH"
    , "wwpQplQQwqVVjqwPjCGCSMCMcHSHvvzHMzvcsrMc"
    , "pfjlQRpPRRLQWtmLNdWdmqqJ"
    , "CPTPPmbjmVjVGCvzbjjPrGsnnMpttdtGdncdMccDRd"
    , "lhlHzQSHwzhJLwgWgpMDMMsDdcDQMDMMns"
    , "BHZghLWwSFBJJBFvzmbfjNZvZmCvmb"
    , "PBGcvvcRwpwNcZcNPpPNcTHGdMtrCWrCCtCLWMtWgbVdMV"
    , "fmsJjnqmmfsjQJnjFzSFSqsqgWrtMttZgMWVMbbVMdbSrLtr"
    , "qQjjZFmfjZhZmwcvPhNpTNBTwN"
    , "HHlVVmmsbbqMsJmVzGSBMSrQQrRrGvvnDn"
    , "PZcphZPPZPhjcpdWgPZhRPfcDSrtDBSGNvtggrQtnvQNGNDn"
    , "dcWwFjpcPhRcCpjwdCPLzHblJbLbzmsmbTwzqH"
    , "hRfzTTfRrTGzhGWTrRrbfcQZQSttWtwddJtvdJJvWSHq"
    , "npjnDjFlpDnFFNMjljCnFMQtHHtqNHNQJwwZZqstNwJJ"
    , "DCjpLjjpVLDMDpVLDLQbbhzBhVrcVgVGQQcz"
    , "LncLBLjCSNrNrNpCLQBBBGwqQwzlzmggvqRqgllmzwtv"
    , "fMZPHhhHfthMdbRgHJzmVqlvwlwg"
    , "hfsPbZFPPDsfGLcBtSFNBSjL"
    , "MlZmszBMJBHrMBMbShwSFpbZSZfwwb"
    , "TCLCcPNGTgTPNGWtCtcWtPcSsRfRjRwjFbfpNFDjwsFspw"
    , "nVtqqsWsdHzJHqmM"
    , "RCrhSmWrmrvmrvhMvRNrRCzCJcQQbPtsMZVGJJtsZssPcQcZ"
    , "jLFBGqLFpqBLgZVbPbsLJQcbsV"
    , "HjDljGFwrRHRRTrS"
    , "GZZhnrwZBwNjRPRCbCbn"
    , "fJtJJpsVfpgNTbVNFTRP"
    , "JJcpLJfLdcWLdplwRdQMBvSqwRhvrG"
    , "wmZDPlRlCDwglgsHtsBvdBHLFLSddr"
    , "VbVMnMftfVjQWFFHdMBdBFMFHr"
    , "zfjtnGqqnjGqfjPcDPlZPlRDzccw"
    , "BRjhfhvRgnTMlFDDJfZzZFFQDZ"
    , "qLdqcNttwwcwwSPSpqLNmrwmrZsGzzDFZGZFzVssrzJGnsQG"
    , "wSNdHScScdmwHSpdNcmmtLMvChRHbvBMTBnCBBvhvlCh"
    , "JgWTPfFPgCPPlCntQSGghHvQnSdQ"
    , "BzvMZvLVQpdQpSZh"
    , "RwVVjRDVcRDNDTlJPqTv"
    , "SGHSrBBRPhPPHQcTccQTRRQjTN"
    , "vvWvspCbzWVWVrWdjj"
    , "wZpDzCDgDbCZJZzJGlrlqPqnqPllmH"
    , "FCncCrDWMLCbjMCcFpLdzZfmZzwwWzdzNRZdWB"
    , "sqsgTqHSqllNldMwlZzJ"
    , "MtHPTgQhvhhqcrDrrDpjLCQc"
    , "pPPvmPWSClqqPvqCmSwqmgGBWDjhGLHfjhDLJGjBBhNj"
    , "zrbdcdMndcRdTrsMcbTRdzRFVHjLjDjNLNHsfDhNGjhJNhDj"
    , "RdFFcnTdZcTrRRdFFbZtwQCPQglvPlwJwQPZSqqP"
    , "wlmbvwmvQvWQsvmbsSsQbswlRCNPfCTcTRVCffPtTSCPNRVP"
    , "FhJJJFgFqJGBtDpJhTTcVcVhdcCdCdTV"
    , "GDFtgLFnqqDGqGZsQvsllrjbLjbrvw"
    , "lnFSnJvmgvLlfnJpgnsjnjgfDQWqCJqZdDtDCtCtCdDrtDDQ"
    , "VTBBMPFcNNtMZDMW"
    , "VTGbzGGhTbTGHwVPvvFnfpvjgHnfjppp"
    , "JJwHqvlvDjljDwJFlZjZDwHNNsMqhNpphNpmNVzpsnsnRV"
    , "mTLgrLLcLSTTTdmPPfrrrnssNhRNWhgngzMWzgzVnM"
    , "SmTfdSBbBJbtjJvljl"
    , "bPNLwTCLLQQqtJsf"
    , "zdnnZVlWWGGRWGWdgdSStQMqJSMRptftbsMf"
    , "FWbvgvZZZZgnTmwrrhrFPCrP"
    , "HcGzzszFGllHWHbZspHbHGsHTwwrTrLLCNjSZwNjNjjCCNLj"
    , "PBJMJQJDDDnDggRhMdRSLmjTmTwwVjVQSvvwvC"
    , "RqfdhgDPDJDqJJnBdfzWWHcstslcbtStfHzl"
    , "zvRRlCqrdNdZcZpjBpVwjsmjsm"
    , "fgbTDqbhGfDnLDnLLqLhFmsHpTPHjHppppBwpwws"
    , "nhnnnDDngDtDbfSbDnGhhgRlNvQdQqNvQvtcQQNJRNJN"
    , "cZbCcbbScCbcmPGjPfSBQQSq"
    , "lnMnnVsMVvmzzGMDzPDf"
    , "LhrTsTTglrnsrrWWVvlwTnNtcpZRCmhtbCZFdttZbRCp"
    , "NWrFPZVWNVrvvrhtnNdddtpldmjm"
    , "DcBQBDsJbCwQnbtdzmjjjljbpjbz"
    , "qCDcGsDJGCcBDBcswJnBJQDfWfqgvZSvgZPfrVSWvPvZZZ"
    , "vcsdHdGtHtMHMFtVsddsWCcbppZwjScLpWhbjRWR"
    , "NTwrnzJrgTPrDwnlphRpjSpWbJJLLZWj"
    , "TlDPfPnzzlzTBzzvQFFBHMtVtqBqqw"
    , "NHnqqfZvZBNHHvgfrSlJrJCSllJRVrCn"
    , "TDTdhLMWjFcddMJPSSPJRmlCPz"
    , "bljWFdLLTDLtdFtLlwZvqfbgwwHfwqHNvw"
    , "BRRjhRQndRNVqBjRVhFLccjpwMmLmjHmgFHH"
    , "fZJfJvzPPWtWWlltZzZPpcgFMsFFwwFdpHdgwtdw"
    , "PCrdrzzfWCPdvSlqTqNSDnnQVVQQGT"
    , "DjbfBMDSfBljBsLSjSZbzrGtPtMCPtVPvvqrzqzG"
    , "mWdJWcppcNTdpppjzjRRVrPRpq"
    , "QncmnHwmdTmwQcmjNTfgfhlBShshhsffnfbB"
    , "WGDsMJsrjHCWtDMGDDVQqSvZqfSJzSnvnvvv"
    , "LgLFLFBFLVVzfBzMqZ"
    , "lgmFcwLhNcwdwwMLwhmcRDjNpCWRsWRspGGssHCp"
    , "PnPzNccnjFfvCvhbSBVcWqdhSVhV"
    , "psGMDQJDDDJgQNDHHJbwqwBsVqqZVWBBhBdd"
    , "DlDJDQGptpgpGDfTRnrTrFPnNTlf"
    , "MSSSMLLmFHcDScSq"
    , "ppZnCsbjPZpnnJcbRDmzHJqRRD"
    , "pmNmnGnQNnClZGMVMdBGrMgVWg"
    , "lsTTGcQzBcljCcQzGcGjGptttpmvSJtmggtwwswwtS"
    , "qZRnrhMbRVdhZRhhdnnVRPbmwSNwNNHtmJBvwpvtwNSvSb"
    , "VnMrqrrdqhZrnrBLLlzzlQjQjLfTcGfFDF"
    , "dJJTlHvhZqZlQTJnSgQDzgsSbScsSBzc"
    , "RRNtGjCCpRPPpRtjfrttRzmbscLsLZLgcsbmLzSGLB"
    , "wfNttfNrtWwPNNFfRtpfrdJMTTTZTMZTTVTlVwTlvM"
    , "PQTGLmdNTgPmGgNNdCPLQlrMqBrDzMCMFqDqFqjVCBCD"
    , "hhRwwvpSFmzDrmFh"
    , "vwwZfSfsmvtSspnZLLLdLGWPTGTQtTWG"
    , "pMcWzWFvWhFpPMWzvvhpdprHTZTQrHrQdZTJdfTgQTnJ"
    , "CGbjBbNjjDmRHJDgrTVVZg"
    , "NNttGlGqNLsbtlhMFMFcMLwMvvZz"
    , "CGSCBNCQBtBCQttBwCGtGtQrqrLrJqZHLHbqHvLDHLrq"
    , "nVVhPMfVdfVPbfqLLqgDDqPvgZsv"
    , "cpVncbfnhFcBltTplpmTBC"
    , "MrdcdStbMnddtRBdqMnFmbqGCwqCVHVsNHwPfGVPqsCsCs"
    , "DBLllzWWQQzlZVVVCsGWHfsH"
    , "JQphjTgBjlLgjjpTpLgvTjQnnnSJJRRFmdbRRSdMRtmdMc"
    , "QbRZMSWMblwLsgpwZzqZ"
    , "BFncBrfcdNrrnVrNjsFzFTJpJLGJsGqLTp"
    , "VjhDDBdrfdhQMllzHmPQMh"
    , "LdVVjFVFbpVGRQGllG"
    , "cNMcJNHzJWJtCWHNJHcHczWpGmmhMQmBBqrlRhBmpGpGBQ"
    , "JZzTTtCZtHCJnNnNwPfbFpnfdDdLdnvP"
    , "TpMlrWTTddjmlmDmgQgRtw"
    , "MNNVMSsVSNSnNVMFLDqwtGgRRtGbgFRwtR"
    , "CCLSCPSCZZHVCfZscBJJhPphpdpprdhjJM"
    , "gSMSHJHsMMpzRgHzsRMPPSzsPhtZtZdqdDqQDhdCdZmQldht"
    , "FCcCnrGcNTfvvtqqfvlflQ"
    , "TrTrWNWwrTJLMzJCzWLL"
    , "TpTzwMrfbrpFpMbFrrrzbPSdZmtSZRTlTZRlmdCVlCtJ"
    , "vqvWgqDJQJsQCVtZgdZdRRGd"
    , "vsvLJLchWBcqnvczwjLfzPjfrjzPrz"
    , "zqzbqCFZgmzzmNmf"
    , "vpRWSbRVbVWddVpwvwdRSwnSNgLHsnfNgMmgMLMmnrns"
    , "DwWVpJRlpdbpRDWdGJGcGlhFtPPCqCCBFqZPQttlqFBq"
    , "wQRlwtBJBDwttJdGvLfBvHLLfTLz"
    , "MMmNZcMrcMFnRHzfjjvvHfvc"
    , "FggpbFnhrNNrrMrMbMbnhQVJVhstJwqWCVCRsQJQ"
    , "DQbCGblQlpQFQlHjCbjwDQQMggNmJmgnnpRBngfZmNgJMf"
    , "zvhWccWVdWBchdssPrrWZZZfmsmmmgsnZZJRsRTf"
    , "zBdtqPccWPHFCqCCqljq"
    , "ttrbRMmgtHgfmHSfBpLfnBBZBppB"
    , "CVTJDCCNPwCPDwcqzmddQZdTQdnLBQThWp"
    , "zwFDjwDJJPzjzVNcVJwCcbRHGmbbMrFHgHvrsgbblG"
    , "gZjjwHqHCzrMZVVR"
    , "hhzcdTzPrVhVCGMb"
    , "fPcmLPNffsccJDdNDjBnpwzmHqgWjHwwvg"
    , "SJQFSvQBlzbSCgdPPddPPPSN"
    , "pcrjcWLwwcHcgPNgTPLMNTCB"
    , "pRsjsWRnrpHRmrBrHrjlbJFvvzQFnzQblQDDbJ"
    , "VjQVMQPVMfVPPbGPHHbGJD"
    , "pcqSttltsbDGddsCJG"
    , "TSchqLtTLFhgQbMMQMrr"
    , "trqzMRwNTtDzLPJQgWmjmjrf"
    , "lbBQdpZbsmhGmZhmmG"
    , "llVbpCplvvHBBHpnRDcDRRqnRRQnFRzT"
    , "SLSSFFmzLShsVSSHnLnrJdbnRdZZbrRw"
    , "qCfWBftpNWNNlqvTpwrRbGGCnwGmgRJGZn"
    , "NcTBNpvWvBWpMftNffpqWlTpmzPDQPSzFVMsFQVhHsjHszss"
    , "VtJtNBRBGDpdpNbC"
    , "QgLncnttvFcwwhLvFjSGsSbmmQCSDdpCmpdG"
    , "vLgjLhhrctMvLFFjLtMTLMgfPZqBZPZzJBBfWZZPRZZTRV"
    , "mJzDJJpJBvfsGMQnBM"
    , "CwPWCLRRWwRqwPqhPsrZrnrlhhQrMTrvZl"
    , "dCdLLSPRLSqWqVSLqLjgJDzDmtbngFVtJtzz"
    , "mtgWtMWrqjzQTTjghwwfczlNJdlcJnlc"
    , "FvRsDPPFGRBFvvslwDnTlcTTdwndlh"
    , "SGBZRBTsFGBRvLpvSCmgQWQjgggMrQjmmSmW"
    , "GcsRrQhrVVjhRcWlnDFGGmvntDWZ"
    , "TPbSgJJgBSCbCTbLHMCMTTZdFHvtZlWZDZFzmzZHZmmF"
    , "gBCMCSpbPMMPjcjqQQpqQprv"
    , "nZJcnZwvwzvTTTVtpDFnHH"
    , "DQPBqGGGdMdTRHRBpNgFNR"
    , "dCGPfhPWQdWWWCWShWPqrChWLLwLswjcvSJbvbLjJLbzJbJD"
    , "QrBQtdtrQBrdtFHPrdQBDvGhLGnPnCWnmpDmLpmD"
    , "NjlRJRlNzJJVbSSRVZwwJcmpWDGCWnbchnLCCmnWCG"
    , "llSJzsZzMMlsSZjSjZwJNQqtHHdBFsqdfTHhqFftQB"
    , "zdTJFHTdDBzrNdMnhNnNdM"
    , "ZlLZZcLtVtcWtGjtzLjLZjCrnVNrnRbrQQbQSRVrRnSNqS"
    , "lZtGtCvjZPCGCctPpsDDBzTHFmPmFszD"
    , "mQSMvdMQtQdZhQrPWCPqPQrN"
    , "RwjwnZGzJFTZgzggzJDDwJnCPPhNNqPrLhrGNcWcWNPqCq"
    , "ZTzDfnwFzTngTwJvfSlMtMMlmsHmHt"
    , "lZlmFRVZWmgQWhRsRpJsCJpJct"
    , "PTbPTGTGwwGrbdfjNNZJvcCsCZtvpTsh"
    , "bGdBBqGrdBPjDMzzVFZgqQzFFL"
    , "szvsmLvppPPtzGLGWpVdTSHTNgjHQRmHTgSH"
    , "FnBMBNZwZNcnDZMcnZlZgwgdQTTHjVJjHHVRQHJj"
    , "DnZrFCMZMNffrLPbLsfW"
    , "rJvmnBgnrCrGRSGNQR"
    , "hthjNfhwctwpjTLtVLjTGSpldSCGSPdlPSRzSqSz"
    , "TVcTfHNFcwtjMhTvgbHZsBbWmmZbnH"
    , "WsQgstQmvQJnssWsWPzhRzhBjZBSBRZSnj"
    , "qwCNqFwDrrlDrFPvRhTSPPzLRz"
    , "bppqwppCddlvfbDNVgmMmtMfVVmfmVWW"
    ]
