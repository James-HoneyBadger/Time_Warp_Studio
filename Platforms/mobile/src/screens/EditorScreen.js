import React from 'react';
import { View, Text, StyleSheet } from 'react-native';
import { useSelector } from 'react-redux';
import { darkTheme } from '../styles/theme';

export default function EditorScreen() {
  const { code, language } = useSelector(state => state.editor);
  const theme = darkTheme;

  return (
    <View style={[styles.container, { backgroundColor: theme.colors.background }]}>
      <View style={[styles.header, { backgroundColor: theme.colors.surface }]}>
        <Text style={[styles.headerText, { color: theme.colors.text }]}>
          {language} Editor
        </Text>
      </View>
      <View style={styles.editorContainer}>
        <Text style={[styles.placeholder, { color: theme.colors.textSecondary }]}>
          Editor Component - Coming Soon
        </Text>
      </View>
    </View>
  );
}

const styles = StyleSheet.create({
  container: {
    flex: 1,
  },
  header: {
    padding: 16,
    borderBottomWidth: 1,
    borderBottomColor: '#374151',
  },
  headerText: {
    fontSize: 18,
    fontWeight: 'bold',
  },
  editorContainer: {
    flex: 1,
    justifyContent: 'center',
    alignItems: 'center',
  },
  placeholder: {
    fontSize: 16,
  },
});
