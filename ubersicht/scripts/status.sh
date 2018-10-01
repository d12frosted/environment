#!/usr/bin/bash

echo $(sh ./scripts/time_script)@$(sh ./scripts/date_script)@$(sh ./scripts/battery_percentage_script)%@$(sh ./scripts/battery_charging_script)@$(sh ./scripts/wifi_status_script)@$(sh ./scripts/get_volume_stat.sh)
