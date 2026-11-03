package com.shaft.intellij.settings

import com.intellij.ui.dsl.builder.panel
import javax.swing.JComponent

/**
 * Kotlin UI DSL chrome for Settings groups Agent / MCP / Keys (W1.5 / Carl F16).
 *
 * Hosts the section headers and help copy; [ShaftSettingsConfigurable] continues to own
 * Password Safe wiring and SearchableConfigurable id `shaft.settings`.
 */
object ShaftSettingsGroupsForm {
    @JvmStatic
    fun createChrome(): JComponent = panel {
        group("Agent") {
            row {
                label("Assistant agent route, model, and local endpoint. Configure details below.")
            }
        }
        group("MCP") {
            row {
                label("MCP stdio command and connection check. Badges come from real verification only.")
            }
        }
        group("Keys") {
            row {
                label("Provider keys stay in Password Safe or environment variables — never in project files.")
            }
        }
    }
}
