import React from 'react';
import { View, Text, StyleSheet } from 'react-native';
import { darkTheme } from '../styles/theme';

export default function FilesScreen() {
  const theme = darkTheme;

  return (
    <View style={[styles.container, { backgroundColor: theme.colors.background }]}>
      <View style={[styles.fileList, { backgroundColor: theme.colors.surface }]}>
        <Text style={[styles.placeholder, { color: theme.colors.textSecondary }]}>
          File Browser - Coming Soon
        </Text>
      </View>
    </View>
  );
}

const styles = StyleSheet.create({
  container: {
    flex: 1,
  },
  fileList: {
    flex: 1,
    margin: 16,
    borderRadius: 8,
    justifyContent: 'center',
    alignItems: 'center',
  },
  placeholder: {
    fontSize: 16,
  },
});
