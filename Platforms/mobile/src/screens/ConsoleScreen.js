import React from 'react';
import { View, Text, StyleSheet } from 'react-native';
import { darkTheme } from '../styles/theme';

export default function ConsoleScreen() {
  const theme = darkTheme;

  return (
    <View style={[styles.container, { backgroundColor: theme.colors.background }]}>
      <View style={[styles.console, { backgroundColor: theme.colors.surface }]}>
        <Text style={[styles.placeholder, { color: theme.colors.textSecondary }]}>
          Console Output - Coming Soon
        </Text>
      </View>
    </View>
  );
}

const styles = StyleSheet.create({
  container: {
    flex: 1,
  },
  console: {
    flex: 1,
    justifyContent: 'center',
    alignItems: 'center',
    margin: 16,
    borderRadius: 8,
  },
  placeholder: {
    fontSize: 16,
  },
});
