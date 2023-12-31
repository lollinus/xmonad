[Unit]
Wants=org.gnome.SettingsDaemon.A11ySettings.target
Wants=org.gnome.SettingsDaemon.Color.target
Wants=org.gnome.SettingsDaemon.Datetime.target
Wants=org.gnome.SettingsDaemon.Housekeeping.target
Wants=org.gnome.SettingsDaemon.Keyboard.target
Wants=org.gnome.SettingsDaemon.MediaKeys.target
Wants=org.gnome.SettingsDaemon.Power.target
Wants=org.gnome.SettingsDaemon.PrintNotifications.target
Wants=org.gnome.SettingsDaemon.Rfkill.target
Wants=org.gnome.SettingsDaemon.ScreensaverProxy.target
Wants=org.gnome.SettingsDaemon.Sharing.target
Wants=org.gnome.SettingsDaemon.Smartcard.target
Wants=org.gnome.SettingsDaemon.Sound.target
Wants=org.gnome.SettingsDaemon.UsbProtection.target
Wants=org.gnome.SettingsDaemon.Wacom.target
Wants=org.gnome.SettingsDaemon.XSettings.target

Requires=gnome-flashback.target
Requires=indicators-pre.target

# here we list the indicators that we want to load
Wants=indicator-application.service
Wants=indicator-bluetooth.service
Wants=indicator-datetime.service
Wants=indicator-keyboard.service
Wants=indicator-messages.service
Wants=indicator-power.service
Wants=indicator-printers.service
Wants=indicator-session.service
Wants=indicator-sound.service