package darecreek.vfuAutotest.alu

import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3._
import chiseltest.WriteVcdAnnotation
import scala.reflect.io.File
import scala.reflect.runtime.universe._
import darecreek.exu.vfu._
import darecreek.exu.vfu.alu._
import darecreek.exu.vfu.VInstructions._
import darecreek.exu.vfu.perm._
import darecreek.exu.vfu.fp._
import darecreek.exu.vfu.div._
import darecreek.exu.vfu.vmask._
import darecreek.exu.vfu.reduction._
import org.chipsalliance.cde.config.Parameters

abstract class TestBehavior(filename : String, ctrlBundle : TestCtrlBundleBase, sign : String, instid : String) extends BundleGenHelper {

    var lmulLsOneDone : Boolean = false

    def setLmulLsOneDone() = {lmulLsOneDone = true}
    def getLmulLsOneDone() = lmulLsOneDone

    def getTestfilePath() : String              = Datapath.testdataRoot + filename
    def getCtrlBundle() : TestCtrlBundleBase    = ctrlBundle
    def getSign() : String                      = sign
    def getInstid() : String                    = instid
    
    def getDut() : Module               = {
        val dut = new VAluWrapper
        return dut
    }

    def getHexfield(simi : Map[String,String], keyname : String) : Int = {
        if(simi.get(keyname) != None) {
            val fflags = simi.get(keyname).get
            var startFrom = 2
            if (fflags.slice(0, 4).equals("0x0x")) startFrom = 4
            // println(s"fflags.slice(0, 4): ${fflags.slice(0, 4)},")
            var parsed = Integer.parseInt(fflags.slice(startFrom, fflags.length), 16)
            // println(keyname, fflags, parsed)
            return parsed
        }
        return 0
    }

    def getImm(simi : Map[String,String]) : Int = {
        var hasImm = simi.get("IMM") != None
        // var hasUimm = simi.get("UIMM") != None
        if (hasImm) return getHexfield(simi, "IMM")
        return getHexfield(simi, "UIMM")
    }

    def getVstart(simi : Map[String,String]) : Int = {
        return getHexfield(simi, "VSTART")
    }

    def getFflags(simi : Map[String,String]) : Int = {
        return getHexfield(simi, "fflags")
    }

    def getfrm(simi : Map[String,String]) : Int = {
        return getHexfield(simi, "frm")
    }

    def dump(simi : Map[String,String], dut_out : String, golden_vd : String, fault_wb : String = "") = {
        //println("fault_wb in TestBehavior", fault_wb)
        TestResults.addResult(TestResults.InstTestRes(
            this.getInstid(),
            true,
            dut_out,
            golden_vd,
            fault_wb
        ))
        Dump.dump(simi, instid, dut_out, golden_vd, fault_wb=fault_wb)
    }

    def test_init(dut : Module) {
        dut match {
            case alu_dut : VAluWrapper => TestHarnessAlu.test_init(alu_dut)
            case mac_dut : VMacWrapper => TestHarnessMac.test_init(mac_dut)
            case mask_dut : VMask => TestHarnessMask.test_init(mask_dut)
            case dut : VFPUWrapper => TestHarnessFPU.test_init(dut)
            case dut : VDivWrapper => TestHarnessDiv.test_init(dut)
            case dut : Reduction => {}
            case perm_dut : Permutation => TestHarnessFSM.test_init(perm_dut)
        }
    }

    def testMultiple(simi:Map[String,String],ctrl:TestCtrlBundleBase,s:String, dut:Module) {
        dut match {
            case alu_dut : VAluWrapper => testMultiple(simi,ctrl.asInstanceOf[CtrlBundle],s, alu_dut)
            case mac_dut : VMacWrapper => testMultiple(simi,ctrl.asInstanceOf[CtrlBundle],s, mac_dut)
            case mask_dut : VMask => testMultiple(simi,ctrl.asInstanceOf[CtrlBundle],s, mask_dut)
            case dut : VFPUWrapper => testMultiple(simi,ctrl.asInstanceOf[CtrlBundle],s, dut)
            case dut : VDivWrapper => testMultiple(simi,ctrl.asInstanceOf[CtrlBundle],s, dut)
            case dut : Reduction => testMultiple(simi,ctrl.asInstanceOf[CtrlBundle],s, dut)
            case perm_dut : Permutation => testMultiple(simi,ctrl.asInstanceOf[CtrlBundle],s, perm_dut)
        }
    }

    def testSingle(simi:Map[String,String],ctrl:TestCtrlBundleBase,s:String, dut:Module) {
        dut match {
            case alu_dut : VAluWrapper => testSingle(simi,ctrl.asInstanceOf[CtrlBundle],s, alu_dut)
            case mac_dut : VMacWrapper => testSingle(simi,ctrl.asInstanceOf[CtrlBundle],s, mac_dut)
            case mask_dut : VMask => testSingle(simi,ctrl.asInstanceOf[CtrlBundle],s, mask_dut)
            case dut : VFPUWrapper => testSingle(simi,ctrl.asInstanceOf[CtrlBundle],s, dut)
            case dut : VDivWrapper => testSingle(simi,ctrl.asInstanceOf[CtrlBundle],s, dut)
            case dut : Reduction => testSingle(simi,ctrl.asInstanceOf[CtrlBundle],s, dut)
            case perm_dut : Permutation => testSingle(simi,ctrl.asInstanceOf[CtrlBundle],s, perm_dut)
        }
    }

    def testMultiple(simi:Map[String,String],ctrl:CtrlBundle,s:String, dut:VAluWrapper) = { println("!!!!!!called unimplemented testMultiple alu")}
    def testSingle(simi:Map[String,String],ctrl:CtrlBundle,s:String, dut:VAluWrapper) = { println("!!!!!!called unimplemented testSingle alu") }
    def testMultiple(simi:Map[String,String],ctrl:CtrlBundle,s:String, dut:Permutation) = { println("!!!!!!called unimplemented testMultiple perm")}
    def testSingle(simi:Map[String,String],ctrl:CtrlBundle,s:String, dut:Permutation) = { println("!!!!!!called unimplemented testSingle perm") }
    def testMultiple(simi:Map[String,String],ctrl:CtrlBundle,s:String, dut:VMacWrapper) = { println("!!!!!!called unimplemented testMultiple mac")}
    def testSingle(simi:Map[String,String],ctrl:CtrlBundle,s:String, dut:VMacWrapper) = { println("!!!!!!called unimplemented testSingle mac") }
    def testMultiple(simi:Map[String,String],ctrl:CtrlBundle,s:String, dut:VMask) = { println("!!!!!!called unimplemented testMultiple mask")}
    def testSingle(simi:Map[String,String],ctrl:CtrlBundle,s:String, dut:VMask) = { println("!!!!!!called unimplemented testSingle mask") }
    def testMultiple(simi:Map[String,String],ctrl:CtrlBundle,s:String, dut:VFPUWrapper) = { println("!!!!!!called unimplemented testMultiple FPU")}
    def testSingle(simi:Map[String,String],ctrl:CtrlBundle,s:String, dut:VFPUWrapper) = { println("!!!!!!called unimplemented testSingle FPU") }
    def testMultiple(simi:Map[String,String],ctrl:CtrlBundle,s:String, dut:VDivWrapper) = { println("!!!!!!called unimplemented testMultiple Div")}
    def testSingle(simi:Map[String,String],ctrl:CtrlBundle,s:String, dut:VDivWrapper) = { println("!!!!!!called unimplemented testSingle Div") }
    def testMultiple(simi:Map[String,String],ctrl:CtrlBundle,s:String, dut:Reduction) = { println("!!!!!!called unimplemented testMultiple Reduction")}
    def testSingle(simi:Map[String,String],ctrl:CtrlBundle,s:String, dut:Reduction) = { println("!!!!!!called unimplemented testSingle Reduction") }

}

// map of string to test behavior class
object TBMap {
    val tbMap = Map(
        ("vmadc_vv" -> (() => new VmadcvvTestBehavior)),
        ("vmadc_vvm" -> (() => new VmadcvvmTestBehavior)),
        ("vmsbc_vv" -> (() => new VmsbcvvTestBehavior)),
        ("vmsbc_vvm" -> (() => new VmsbcvvmTestBehavior)),
        ("vmseq_vv" -> (() => new VmseqvvTestBehavior)),
        ("vmsne_vv" -> (() => new VmsnevvTestBehavior)),
        ("vmsltu_vv" -> (() => new VmsltuvvTestBehavior)),
        ("vmsleu_vv" -> (() => new VmsleuvvTestBehavior)),
        ("vmslt_vv" -> (() => new VmsltvvTestBehavior)),
        ("vmsle_vv" -> (() => new VmslevvTestBehavior)),
        ("vmsgtu_vx" -> (() => new VmsgtuvxTestBehavior)),
        ("vmsgt_vx" -> (() => new VmsgtvxTestBehavior)),
        ("vmadc_vx" -> (() => new VmadcvxTestBehavior)),
        ("vmadc_vxm" -> (() => new VmadcvxmTestBehavior)),
        ("vmsbc_vx" -> (() => new VmsbcvxTestBehavior)),
        ("vmsbc_vxm" -> (() => new VmsbcvxmTestBehavior)),
        ("vmseq_vx" -> (() => new VmseqvxTestBehavior)),
        ("vmsne_vx" -> (() => new VmsnevxTestBehavior)),
        ("vmsltu_vx" -> (() => new VmsltuvxTestBehavior)),
        ("vmsleu_vx" -> (() => new VmsleuvxTestBehavior)),
        ("vmslt_vx" -> (() => new VmsltvxTestBehavior)),
        ("vmsle_vx" -> (() => new VmslevxTestBehavior)),
        ("vmsgtu_vi" -> (() => new VmsgtuviTestBehavior)),
        ("vmseq_vi" -> (() => new VmseqviTestBehavior)),
        ("vmsleu_vi" -> (() => new VmsleuviTestBehavior)),
        ("vmsgt_vi" -> (() => new VmsgtviTestBehavior)),
        ("vmsne_vi" -> (() => new VmsneviTestBehavior)),
        ("vmsle_vi" -> (() => new VmsleviTestBehavior)),
        ("vmadc_vi" -> (() => new VmadcviTestBehavior)),
        ("vmadc_vim" -> (() => new VmadcvimTestBehavior)),
        ("vmv1r_v" -> (() => new Vmv1rvTestBehavior)),
        ("vmv2r_v" -> (() => new Vmv2rvTestBehavior)),
        ("vmv4r_v" -> (() => new Vmv4rvTestBehavior)),
        ("vmv8r_v" -> (() => new Vmv8rvTestBehavior)),
        ("vmv_s_x" -> (() => new VmvsxTestBehavior)),
        ("vfmv_s_f" -> (() => new VfmvsfTestBehavior)),
        ("vnclip_wv" -> (() => new VnclipwvTestBehavior)),
        ("vnclipu_wv" -> (() => new VnclipuwvTestBehavior)),
        ("vnsrl_wv" -> (() => new VnsrlwvTestBehavior)),
        ("vnsra_wv" -> (() => new VnsrawvTestBehavior)),
        ("vnclip_wx" -> (() => new VnclipwxTestBehavior)),
        ("vnclipu_wx" -> (() => new VnclipuwxTestBehavior)),
        ("vnsrl_wx" -> (() => new VnsrlwxTestBehavior)),
        ("vnsra_wx" -> (() => new VnsrawxTestBehavior)),
        ("vnclip_wi" -> (() => new VnclipwiTestBehavior)),
        ("vnclipu_wi" -> (() => new VnclipuwiTestBehavior)),
        ("vnsrl_wi" -> (() => new VnsrlwiTestBehavior)),
        ("vnsra_wi" -> (() => new VnsrawiTestBehavior)),
        ("vadd_vv" -> (() => new VaddvvTestBehavior)),
        ("vsub_vv" -> (() => new VsubvvTestBehavior)),
        ("vand_vv" -> (() => new VandvvTestBehavior)),
        ("vor_vv" -> (() => new VorvvTestBehavior)),
        ("vxor_vv" -> (() => new VxorvvTestBehavior)),
        ("vsll_vv" -> (() => new VsllvvTestBehavior)),
        ("vsrl_vv" -> (() => new VsrlvvTestBehavior)),
        ("vsra_vv" -> (() => new VsravvTestBehavior)),
        ("vmax_vv" -> (() => new VmaxvvTestBehavior)),
        ("vmaxu_vv" -> (() => new VmaxuvvTestBehavior)),
        ("vmin_vv" -> (() => new VminvvTestBehavior)),
        ("vminu_vv" -> (() => new VminuvvTestBehavior)),
        ("vmerge_vvm" -> (() => new VmergevvmTestBehavior)),
        ("vmv_v_v" -> (() => new VmvvvTestBehavior)),
        ("vadc_vvm" -> (() => new VadcvvmTestBehavior)),
        ("vsbc_vvm" -> (() => new VsbcvvmTestBehavior)),
        ("vssub_vv" -> (() => new VssubvvTestBehavior)),
        ("vssubu_vv" -> (() => new VssubuvvTestBehavior)),
        ("vsadd_vv" -> (() => new VsaddvvTestBehavior)),
        ("vsaddu_vv" -> (() => new VsadduvvTestBehavior)),
        ("vssrl_vv" -> (() => new VssrlvvTestBehavior)),
        ("vssra_vv" -> (() => new VssravvTestBehavior)),
        ("vaadd_vv" -> (() => new VaaddvvTestBehavior)),
        ("vaaddu_vv" -> (() => new VaadduvvTestBehavior)),
        ("vasub_vv" -> (() => new VasubvvTestBehavior)),
        ("vasubu_vv" -> (() => new VasubuvvTestBehavior)),
        ("vadd_vx" -> (() => new VaddvxTestBehavior)),
        ("vsub_vx" -> (() => new VsubvxTestBehavior)),
        ("vand_vx" -> (() => new VandvxTestBehavior)),
        ("vor_vx" -> (() => new VorvxTestBehavior)),
        ("vxor_vx" -> (() => new VxorvxTestBehavior)),
        ("vsll_vx" -> (() => new VsllvxTestBehavior)),
        ("vsrl_vx" -> (() => new VsrlvxTestBehavior)),
        ("vsra_vx" -> (() => new VsravxTestBehavior)),
        ("vmax_vx" -> (() => new VmaxvxTestBehavior)),
        ("vmaxu_vx" -> (() => new VmaxuvxTestBehavior)),
        ("vmin_vx" -> (() => new VminvxTestBehavior)),
        ("vminu_vx" -> (() => new VminuvxTestBehavior)),
        ("vmerge_vxm" -> (() => new VmergevxmTestBehavior)),
        ("vadc_vxm" -> (() => new VadcvxmTestBehavior)),
        ("vsbc_vxm" -> (() => new VsbcvxmTestBehavior)),
        ("vssub_vx" -> (() => new VssubvxTestBehavior)),
        ("vssubu_vx" -> (() => new VssubuvxTestBehavior)),
        ("vsadd_vx" -> (() => new VsaddvxTestBehavior)),
        ("vsaddu_vx" -> (() => new VsadduvxTestBehavior)),
        ("vssrl_vx" -> (() => new VssrlvxTestBehavior)),
        ("vssra_vx" -> (() => new VssravxTestBehavior)),
        ("vaadd_vx" -> (() => new VaaddvxTestBehavior)),
        ("vaaddu_vx" -> (() => new VaadduvxTestBehavior)),
        ("vasub_vx" -> (() => new VasubvxTestBehavior)),
        ("vasubu_vx" -> (() => new VasubuvxTestBehavior)),
        ("vadd_vi" -> (() => new VaddviTestBehavior)),
        ("vand_vi" -> (() => new VandviTestBehavior)),
        ("vor_vi" -> (() => new VorviTestBehavior)),
        ("vxor_vi" -> (() => new VxorviTestBehavior)),
        ("vsadd_vi" -> (() => new VsaddviTestBehavior)),
        ("vsll_vi" -> (() => new VsllviTestBehavior)),
        ("vsrl_vi" -> (() => new VsrlviTestBehavior)),
        ("vsra_vi" -> (() => new VsraviTestBehavior)),
        ("vssrl_vi" -> (() => new VssrlviTestBehavior)),
        ("vssra_vi" -> (() => new VssraviTestBehavior)),
        ("vsaddu_vi" -> (() => new VsadduviTestBehavior)),
        ("vrsub_vx" -> (() => new VrsubvxTestBehavior)),
        ("vrsub_vi" -> (() => new VrsubviTestBehavior)),
        ("vmv_v_x" -> (() => new VmvvxTestBehavior)),
        ("vmv_x_s" -> (() => new VmvxsTestBehavior)),
        ("vmv_v_i" -> (() => new VmvviTestBehavior)),
        ("vfmv_f_s" -> (() => new VfmvfsTestBehavior)),
        ("vmerge_vim" -> (() => new VmergevimTestBehavior)),
        ("vadc_vim" -> (() => new VadcvimTestBehavior)),
        ("vwadd_wv" -> (() => new VwaddwvTestBehavior)),
        ("vwaddu_wv" -> (() => new VwadduwvTestBehavior)),
        ("vwsub_wv" -> (() => new VwsubwvTestBehavior)),
        ("vwsubu_wv" -> (() => new VwsubuwvTestBehavior)),
        ("vwadd_vv" -> (() => new VwaddvvTestBehavior)),
        ("vwaddu_vv" -> (() => new VwadduvvTestBehavior)),
        ("vwsub_vv" -> (() => new VwsubvvTestBehavior)),
        ("vwsubu_vv" -> (() => new VwsubuvvTestBehavior)),
        ("vwadd_wx" -> (() => new VwaddwxTestBehavior)),
        ("vwaddu_wx" -> (() => new VwadduwxTestBehavior)),
        ("vwsub_wx" -> (() => new VwsubwxTestBehavior)),
        ("vwsubu_wx" -> (() => new VwsubuwxTestBehavior)),
        ("vwadd_vx" -> (() => new VwaddvxTestBehavior)),
        ("vwaddu_vx" -> (() => new VwadduvxTestBehavior)),
        ("vwsub_vx" -> (() => new VwsubvxTestBehavior)),
        ("vwsubu_vx" -> (() => new VwsubuvxTestBehavior)),
        ("vzext_vf2" -> (() => new VzextVf2TestBehavior)),
        ("vsext_vf2" -> (() => new VsextVf2TestBehavior)),
        ("vzext_vf4" -> (() => new VzextVf4TestBehavior)),
        ("vsext_vf4" -> (() => new VsextVf4TestBehavior)),
        ("vzext_vf8" -> (() => new VzextVf8TestBehavior)),
        ("vsext_vf8" -> (() => new VsextVf8TestBehavior)),
        ("vdiv_vv" -> (() => new VdivvvTestBehavior)),
        ("vdivu_vv" -> (() => new VdivuvvTestBehavior)),
        ("vdiv_vx" -> (() => new VdivvxTestBehavior)),
        ("vdivu_vx" -> (() => new VdivuvxTestBehavior)),
        ("vrem_vv" -> (() => new VremvvTestBehavior)),
        ("vremu_vv" -> (() => new VremuvvTestBehavior)),
        ("vrem_vx" -> (() => new VremvxTestBehavior)),
        ("vremu_vx" -> (() => new VremuvxTestBehavior)),
        ("vfdiv_vv" -> (() => new VfdivvvTestBehavior)),
        ("vfdiv_vf" -> (() => new VfdivvfTestBehavior)),
        ("vfrdiv_vf" -> (() => new VfrdivvfTestBehavior)),
        ("vfsqrt_v" -> (() => new VfsqrtvTestBehavior)),
        ("vfredosum_vs" -> (() => new VfredosumvsTestBehavior)),
        ("vfredusum_vs" -> (() => new VfredusumvsTestBehavior)),
        ("vfredmax_vs" -> (() => new VfredmaxvsTestBehavior)),
        ("vfredmin_vs" -> (() => new VfredminvsTestBehavior)),
        ("vfwredosum_vs" -> (() => new VfwredosumvsTestBehavior)),
        ("vfwredusum_vs" -> (() => new VfwredusumvsTestBehavior)),
        ("vfncvt_xu_f_w" -> (() => new VfncvtxufwTestBehavior)),
        ("vfncvt_x_f_w" -> (() => new VfncvtxfwTestBehavior)),
        ("vfncvt_f_xu_w" -> (() => new VfncvtfxuwTestBehavior)),
        ("vfncvt_f_x_w" -> (() => new VfncvtfxwTestBehavior)),
        ("vfncvt_f_f_w" -> (() => new VfncvtffwTestBehavior)),
        ("vfncvt_rod_f_f_w" -> (() => new VfncvtrodffwTestBehavior)),
        ("vfncvt_rtz_xu_f_w" -> (() => new VfncvtrtzxufwTestBehavior)),
        ("vfncvt_rtz_x_f_w" -> (() => new VfncvtrtzxfwTestBehavior)),
        ("vfadd_vv" -> (() => new VfaddvvTestBehavior)),
        ("vfsub_vv" -> (() => new VfsubvvTestBehavior)),
        ("vfmul_vv" -> (() => new VfmulvvTestBehavior)),
        ("vfmacc_vv" -> (() => new VfmaccvvTestBehavior)),
        ("vfnmacc_vv" -> (() => new VfnmaccvvTestBehavior)),
        ("vfmsac_vv" -> (() => new VfmsacvvTestBehavior)),
        ("vfnmsac_vv" -> (() => new VfnmsacvvTestBehavior)),
        ("vfmadd_vv" -> (() => new VfmaddvvTestBehavior)),
        ("vfnmadd_vv" -> (() => new VfnmaddvvTestBehavior)),
        ("vfmsub_vv" -> (() => new VfmsubvvTestBehavior)),
        ("vfnmsub_vv" -> (() => new VfnmsubvvTestBehavior)),
        ("vfmin_vv" -> (() => new VfminvvTestBehavior)),
        ("vfmax_vv" -> (() => new VfmaxvvTestBehavior)),
        ("vmfeq_vv" -> (() => new VmfeqvvTestBehavior)),
        ("vmfne_vv" -> (() => new VmfnevvTestBehavior)),
        ("vmflt_vv" -> (() => new VmfltvvTestBehavior)),
        ("vmfle_vv" -> (() => new VmflevvTestBehavior)),
        ("vfsgnj_vv" -> (() => new VfsgnjvvTestBehavior)),
        ("vfsgnjn_vv" -> (() => new VfsgnjnvvTestBehavior)),
        ("vfsgnjx_vv" -> (() => new VfsgnjxvvTestBehavior)),
        ("vfadd_vf" -> (() => new VfaddvfTestBehavior)),
        ("vfsub_vf" -> (() => new VfsubvfTestBehavior)),
        ("vfmul_vf" -> (() => new VfmulvfTestBehavior)),
        ("vfmacc_vf" -> (() => new VfmaccvfTestBehavior)),
        ("vfnmacc_vf" -> (() => new VfnmaccvfTestBehavior)),
        ("vfmsac_vf" -> (() => new VfmsacvfTestBehavior)),
        ("vfnmsac_vf" -> (() => new VfnmsacvfTestBehavior)),
        ("vfmadd_vf" -> (() => new VfmaddvfTestBehavior)),
        ("vfnmadd_vf" -> (() => new VfnmaddvfTestBehavior)),
        ("vfmsub_vf" -> (() => new VfmsubvfTestBehavior)),
        ("vfnmsub_vf" -> (() => new VfnmsubvfTestBehavior)),
        ("vfmin_vf" -> (() => new VfminvfTestBehavior)),
        ("vfmax_vf" -> (() => new VfmaxvfTestBehavior)),
        ("vmfeq_vf" -> (() => new VmfeqvfTestBehavior)),
        ("vmfne_vf" -> (() => new VmfnevfTestBehavior)),
        ("vmflt_vf" -> (() => new VmfltvfTestBehavior)),
        ("vmfle_vf" -> (() => new VmflevfTestBehavior)),
        ("vfsgnj_vf" -> (() => new VfsgnjvfTestBehavior)),
        ("vfsgnjn_vf" -> (() => new VfsgnjnvfTestBehavior)),
        ("vfsgnjx_vf" -> (() => new VfsgnjxvfTestBehavior)),
        ("vfrsub_vf" -> (() => new VfrsubvfTestBehavior)),
        ("vmfgt_vf" -> (() => new VmfgtvfTestBehavior)),
        ("vmfge_vf" -> (() => new VmfgevfTestBehavior)),
        ("vfmerge_vfm" -> (() => new VfmergevfmTestBehavior)),
        ("vfmv_v_f" -> (() => new VfmvTestBehavior)),
        ("vfrsqrt7_v" -> (() => new Vfrsqrt7vTestBehavior)),
        ("vfrec7_v" -> (() => new Vfrec7vTestBehavior)),
        ("vfclass_v" -> (() => new VfclassvTestBehavior)),
        ("vfcvt_xu_f_v" -> (() => new VfcvtxufvTestBehavior)),
        ("vfcvt_x_f_v" -> (() => new VfcvtxfvTestBehavior)),
        ("vfcvt_f_xu_v" -> (() => new VfcvtfxuvTestBehavior)),
        ("vfcvt_f_x_v" -> (() => new VfcvtfxvTestBehavior)),
        ("vfcvt_rtz_xu_f_v" -> (() => new VfcvtrtzxufvTestBehavior)),
        ("vfcvt_rtz_x_f_v" -> (() => new VfcvtrtzxfvTestBehavior)),
        ("vfwadd_wv" -> (() => new VfwaddwvTestBehavior)),
        ("vfwsub_wv" -> (() => new VfwsubwvTestBehavior)),
        ("vfwadd_wf" -> (() => new VfwaddwfTestBehavior)),
        ("vfwsub_wf" -> (() => new VfwsubwfTestBehavior)),
        ("vfwadd_vv" -> (() => new VfwaddvvTestBehavior)),
        ("vfwsub_vv" -> (() => new VfwsubvvTestBehavior)),
        ("vfwadd_vf" -> (() => new VfwaddvfTestBehavior)),
        ("vfwsub_vf" -> (() => new VfwsubvfTestBehavior)),
        ("vfwmacc_vv" -> (() => new VfwmaccvvTestBehavior)),
        ("vfwnmacc_vv" -> (() => new VfwnmaccvvTestBehavior)),
        ("vfwmsac_vv" -> (() => new VfwmsacvvTestBehavior)),
        ("vfwnmsac_vv" -> (() => new VfwnmsacvvTestBehavior)),
        ("vfwmul_vv" -> (() => new VfwmulvvTestBehavior)),
        ("vfwmacc_vf" -> (() => new VfwmaccvfTestBehavior)),
        ("vfwnmacc_vf" -> (() => new VfwnmaccvfTestBehavior)),
        ("vfwmsac_vf" -> (() => new VfwmsacvfTestBehavior)),
        ("vfwnmsac_vf" -> (() => new VfwnmsacvfTestBehavior)),
        ("vfwmul_vf" -> (() => new VfwmulvfTestBehavior)),
        ("vfwcvt_xu_f_v" -> (() => new VfwcvtxufvTestBehavior)),
        ("vfwcvt_x_f_v" -> (() => new VfwcvtxfvTestBehavior)),
        ("vfwcvt_f_xu_v" -> (() => new VfwcvtfxuvTestBehavior)),
        ("vfwcvt_f_x_v" -> (() => new VfwcvtfxvTestBehavior)),
        ("vfwcvt_f_f_v" -> (() => new VfwcvtffvTestBehavior)),
        ("vfwcvt_rtz_xu_f_v" -> (() => new VfwcvtrtzxufvTestBehavior)),
        ("vfwcvt_rtz_x_f_v" -> (() => new VfwcvtrtzxfvTestBehavior)),
        ("vmacc_vv" -> (() => new VmaccvvTestBehavior)),
        ("vnmsac_vv" -> (() => new VnmsacvvTestBehavior)),
        ("vmadd_vv" -> (() => new VmaddvvTestBehavior)),
        ("vnmsub_vv" -> (() => new VnmsubvvTestBehavior)),
        ("vmacc_vx" -> (() => new VmaccvxTestBehavior)),
        ("vnmsac_vx" -> (() => new VnmsacvxTestBehavior)),
        ("vmadd_vx" -> (() => new VmaddvxTestBehavior)),
        ("vnmsub_vx" -> (() => new VnmsubvxTestBehavior)),
        ("vmul_vv" -> (() => new VmulvvTestBehavior)),
        ("vmulh_vv" -> (() => new VmulhvvTestBehavior)),
        ("vmulhu_vv" -> (() => new VmulhuvvTestBehavior)),
        ("vmulhsu_vv" -> (() => new VmulhsuvvTestBehavior)),
        ("vmul_vx" -> (() => new VmulvxTestBehavior)),
        ("vmulh_vx" -> (() => new VmulhvxTestBehavior)),
        ("vmulhu_vx" -> (() => new VmulhuvxTestBehavior)),
        ("vmulhsu_vx" -> (() => new VmulhsuvxTestBehavior)),
        ("vsmul_vv" -> (() => new VsmulvvTestBehavior)),
        ("vsmul_vx" -> (() => new VsmulvxTestBehavior)),
        ("vwmul_vv" -> (() => new VwmulvvTestBehavior)),
        ("vwmulu_vv" -> (() => new VwmuluvvTestBehavior)),
        ("vwmulsu_vv" -> (() => new VwmulsuvvTestBehavior)),
        ("vwmacc_vv" -> (() => new VwmaccvvTestBehavior)),
        ("vwmaccu_vv" -> (() => new VwmaccuvvTestBehavior)),
        ("vwmaccsu_vv" -> (() => new VwmaccsuvvTestBehavior)),
        ("vwmul_vx" -> (() => new VwmulvxTestBehavior)),
        ("vwmulu_vx" -> (() => new VwmuluvxTestBehavior)),
        ("vwmulsu_vx" -> (() => new VwmulsuvxTestBehavior)),
        ("vwmacc_vx" -> (() => new VwmaccvxTestBehavior)),
        ("vwmaccu_vx" -> (() => new VwmaccuvxTestBehavior)),
        ("vwmaccsu_vx" -> (() => new VwmaccsuvxTestBehavior)),
        ("vwmaccus_vx" -> (() => new VwmaccusvxTestBehavior)),
        ("vcpop_m" -> (() => new VcpopmTestBehavior)),
        ("vid_v" -> (() => new VidvTestBehavior)),
        ("viota_m" -> (() => new ViotamTestBehavior)),
        ("vfirst_m" -> (() => new VfirstmTestBehavior)),
        ("vmsbf_m" -> (() => new VmsbfmTestBehavior)),
        ("vmsif_m" -> (() => new VmsifmTestBehavior)),
        ("vmsof_m" -> (() => new VmsofmTestBehavior)),
        ("vmand_mm" -> (() => new VmandmmTestBehavior)),
        ("vmnand_mm" -> (() => new VmnandmmTestBehavior)),
        ("vmandn_mm" -> (() => new VmandnmmTestBehavior)),
        ("vmxor_mm" -> (() => new VmxormmTestBehavior)),
        ("vmor_mm" -> (() => new VmormmTestBehavior)),
        ("vmnor_mm" -> (() => new VmnormmTestBehavior)),
        ("vmorn_mm" -> (() => new VmornmmTestBehavior)),
        ("vmxnor_mm" -> (() => new VmxnormmTestBehavior)),
        ("vslideup_vx" -> (() => new VslideupvxFSMTestBehavior)),
        ("vslidedown_vx" -> (() => new VslidedownvxFSMTestBehavior)),
        ("vslide1up_vx" -> (() => new Vslide1upvxFSMTestBehavior)),
        ("vslide1down_vx" -> (() => new Vslide1downvxFSMTestBehavior)),
        ("vrgather_vv" -> (() => new VrgathervvFSMTestBehavior)),
        ("vrgather_vx" -> (() => new VrgathervxFSMTestBehavior)),
        ("vrgatherei16_vv" -> (() => new Vrgatherei16vvFSMTestBehavior)),
        ("vcompress_vm" -> (() => new VcompressvmFSMTestBehavior)),
        ("vslideup_vi" -> (() => new VslideupviFSMTestBehavior)),
        ("vslidedown_vi" -> (() => new VslidedownviFSMTestBehavior)),
        ("vrgather_vi" -> (() => new VrgatherviFSMTestBehavior)),
        ("vfslide1up_vf" -> (() => new Vfslide1upvfFSMTestBehavior)),
        ("vfslide1down_vf" -> (() => new Vfslide1downvfFSMTestBehavior)),
        ("vredsum_vs" -> (() => new VredsumvsTestBehavior)),
        ("vredmaxu_vs" -> (() => new VredmaxuvsTestBehavior)),
        ("vredmax_vs" -> (() => new VredmaxvsTestBehavior)),
        ("vredminu_vs" -> (() => new VredminuvsTestBehavior)),
        ("vredmin_vs" -> (() => new VredminvsTestBehavior)),
        ("vredand_vs" -> (() => new VredandvsTestBehavior)),
        ("vredor_vs" -> (() => new VredorvsTestBehavior)),
        ("vredxor_vs" -> (() => new VredxorvsTestBehavior)),
        ("vwredsumu_vs" -> (() => new VwredsumuvsTestBehavior)),
        ("vwredsum_vs" -> (() => new VwredsumvsTestBehavior)),

    )
}