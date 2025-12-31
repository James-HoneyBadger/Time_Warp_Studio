import React from 'react';
import { View, Text, StyleSheet, Switch } from 'react-native';
import { useDispatch, useSelector } from 'react-redux';
import { setTheme, setAutoSave, setAutoSync } from '../store/slices/settingsSlice';
import { darkTheme } from '../styles/theme';

export default function SettingsScreen() {
  const dispatch = useDispatch();
  const settings = useSelector(state => state.settings);
  const theme = darkTheme;

  return (
    <View style={[styles.container, { backgroundColor: theme.colors.background }]}>
      <View style={[styles.settingsPanel, { backgroundColor: theme.colors.surface }]}>
        <View style={styles.settingRow}>
          <Text style={[styles.settingLabel, { color: theme.colors.text }]}>
            Auto-Save
          </Text>
          <Switch
            value={settings.autoSave}
            onValueChange={(value) => dispatch(setAutoSave(value))}
            trackColor={{ false: theme.colors.border, true: theme.colors.primary }}
          />
        </View>

        <View style={styles.settingRow}>
          <Text style={[styles.settingLabel, { color: theme.colors.text }]}>
            Auto-Sync
          </Text>
          <Switch
            value={settings.autoSync}
            onValueChange={(value) => dispatch(setAutoSync(value))}
            trackColor={{ false: theme.colors.border, true: theme.colors.primary }}
          />
        </View>

        <Text style={[styles.placeholder, { color: theme.colors.textSecondary }]}>
          More settings coming soon...
        </Text>
      </View>
    </View>
  );
}

const styles = StyleSheet.create({
  container: {
    flex: 1,
  },
  settingsPanel: {
    flex: 1,
    margin: 16,
    borderRadius: 8,
    padding: 16,
  },
  settingRow: {
    flexDirection: 'row',
    justifyContent: 'space-between',
    alignItems: 'center',
    paddingVertical: 12,
    borderBottomWidth: 1,
    borderBottomColor: '#374151',
  },
  settingLabel: {
    fontSize: 16,
    fontWeight: '500',
  },
  placeholder: {
    fontSize: 14,
    marginTop: 24,
  },
});
