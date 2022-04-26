package magicore.testutil

import spinal.core._
import spinal.lib._

object TestSyncResetSpinalConfig
    extends SpinalConfig(
      defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC)
    )
