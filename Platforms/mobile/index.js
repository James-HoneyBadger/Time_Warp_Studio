import React, { useEffect, useState } from 'react';
import { NavigationContainer } from '@react-navigation/native';
import { createBottomTabNavigator } from '@react-navigation/bottom-tabs';
import { Provider as ReduxProvider } from 'react-redux';
import { StatusBar } from 'expo-status-bar';
import AsyncStorage from '@react-native-async-storage/async-storage';
import MaterialCommunityIcons from 'react-native-vector-icons/MaterialCommunityIcons';

import store from './src/store/store';
import { setTheme } from './src/store/actions';

// Screens
import EditorScreen from './src/screens/EditorScreen';
import ConsoleScreen from './src/screens/ConsoleScreen';
import FilesScreen from './src/screens/FilesScreen';
import CloudScreen from './src/screens/CloudScreen';
import SettingsScreen from './src/screens/SettingsScreen';

// Styles
import { lightTheme, darkTheme } from './src/styles/theme';

const Tab = createBottomTabNavigator();

export default function App() {
  const [isReady, setIsReady] = useState(false);
  const [themeMode, setThemeMode] = useState('dark');

  useEffect(() => {
    restoreState();
  }, []);

  const restoreState = async () => {
    try {
      // Load saved theme preference
      const savedTheme = await AsyncStorage.getItem('theme');
      if (savedTheme) {
        setThemeMode(savedTheme);
        store.dispatch(setTheme(savedTheme));
      }

      // Load user authentication state
      const userToken = await AsyncStorage.getItem('userToken');
      if (userToken) {
        // Verify token validity
        // If expired, user will need to login again
      }

      setIsReady(true);
    } catch (e) {
      console.error('Failed to restore state:', e);
      setIsReady(true);
    }
  };

  if (!isReady) {
    return null;
  }

  const theme = themeMode === 'dark' ? darkTheme : lightTheme;

  return (
    <ReduxProvider store={store}>
      <NavigationContainer>
        <Tab.Navigator
          screenOptions={({ route }) => ({
            headerShown: true,
            tabBarActiveTintColor: '#6366f1',
            tabBarInactiveTintColor: '#888888',
            tabBarStyle: {
              backgroundColor: theme.colors.background,
              borderTopColor: theme.colors.border,
            },
            headerStyle: {
              backgroundColor: theme.colors.background,
              borderBottomColor: theme.colors.border,
            },
            headerTintColor: theme.colors.text,
            headerTitleStyle: {
              fontWeight: 'bold',
            },
            tabBarIcon: ({ color, size }) => {
              let iconName;

              switch (route.name) {
                case 'Editor':
                  iconName = 'file-document-edit';
                  break;
                case 'Console':
                  iconName = 'console';
                  break;
                case 'Files':
                  iconName = 'folder-open';
                  break;
                case 'Cloud':
                  iconName = 'cloud-sync';
                  break;
                case 'Settings':
                  iconName = 'cog';
                  break;
                default:
                  iconName = 'alert-circle';
              }

              return (
                <MaterialCommunityIcons name={iconName} size={size} color={color} />
              );
            },
          })}
        >
          <Tab.Screen
            name="Editor"
            component={EditorScreen}
            options={{
              title: 'Editor',
            }}
          />
          <Tab.Screen
            name="Console"
            component={ConsoleScreen}
            options={{
              title: 'Console',
            }}
          />
          <Tab.Screen
            name="Files"
            component={FilesScreen}
            options={{
              title: 'Files',
            }}
          />
          <Tab.Screen
            name="Cloud"
            component={CloudScreen}
            options={{
              title: 'Cloud',
            }}
          />
          <Tab.Screen
            name="Settings"
            component={SettingsScreen}
            options={{
              title: 'Settings',
            }}
          />
        </Tab.Navigator>
      </NavigationContainer>

      <StatusBar style="auto" />
    </ReduxProvider>
  );
}
