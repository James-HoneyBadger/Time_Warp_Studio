/**
 * Mobile Network Manager
 * Handles network resilience, connection quality detection, and adaptive syncing
 */

import { AppState, Platform } from 'react-native'
import NetInfo from '@react-native-community/netinfo'

export class MobileNetworkManager {
  constructor(store) {
    this.store = store
    this.connectionState = null
    this.isSlowNetwork = false
    this.retryCount = 0
    this.maxRetries = 5
    this.baseRetryDelay = 1000 // 1 second
    this.networkSubscription = null
    this.appStateSubscription = null
    this.pingIntervalId = null
    this.lastSuccessfulSync = Date.now()
    this.syncTimeoutId = null
  }

  /**
   * Initialize network monitoring
   */
  async initialize() {
    try {
      // Get initial network state
      const state = await NetInfo.fetch()
      this.updateNetworkState(state)

      // Subscribe to network changes
      this.networkSubscription = NetInfo.addEventListener((state) => {
        this.updateNetworkState(state)
      })

      // Monitor app state
      this.appStateSubscription = AppState.addEventListener(
        'change',
        this.handleAppStateChange.bind(this)
      )

      // Start ping to detect network quality
      this.startNetworkQualityMonitoring()
    } catch (error) {
      console.error('Failed to initialize network manager:', error)
    }
  }

  /**
   * Update network state
   */
  updateNetworkState(state) {
    const { isConnected, type } = state
    const wasOnline = this.store.isOnline

    this.connectionState = state

    // Detect slow networks
    const isSlowNetwork = type === '4g' || type === '3g' || type === '2g'
    this.isSlowNetwork = isSlowNetwork

    // Update store
    if (isConnected !== wasOnline) {
      this.store.setOnlineStatus(isConnected)

      if (isConnected) {
        // Coming online - sync immediately
        this.handleOnlineReconnect()
      } else {
        // Going offline
        this.handleOffline()
      }
    }

    // Adjust sync frequency based on network type
    this.adjustSyncFrequency(type, isSlowNetwork)
  }

  /**
   * Handle app state changes
   */
  handleAppStateChange(nextAppState) {
    if (nextAppState === 'active') {
      // App came to foreground
      this.handleAppForeground()
    } else {
      // App went to background
      this.handleAppBackground()
    }
  }

  /**
   * Handle coming online
   */
  async handleOnlineReconnect() {
    console.log('Network reconnected - starting sync')

    // Reset retry counter
    this.retryCount = 0

    // Sync with backoff
    await this.syncWithBackoff()

    // Resume normal sync loop
    this.startSyncLoop()
  }

  /**
   * Handle going offline
   */
  handleOffline() {
    console.log('Network disconnected')

    // Clear sync timeout
    if (this.syncTimeoutId) {
      clearTimeout(this.syncTimeoutId)
    }

    // Stop aggressive syncing
    if (this.pingIntervalId) {
      clearInterval(this.pingIntervalId)
    }
  }

  /**
   * Handle app coming to foreground
   */
  async handleAppForeground() {
    console.log('App came to foreground')

    // Check network state
    const state = await NetInfo.fetch()
    if (state.isConnected && this.lastSuccessfulSync) {
      const timeSinceLastSync = Date.now() - this.lastSuccessfulSync

      // If more than 30 seconds since last sync, sync now
      if (timeSinceLastSync > 30000) {
        await this.store.sync()
      }
    }

    // Resume sync loop
    this.startSyncLoop()
  }

  /**
   * Handle app going to background
   */
  handleAppBackground() {
    console.log('App going to background')

    // Stop aggressive syncing
    if (this.syncTimeoutId) {
      clearTimeout(this.syncTimeoutId)
    }

    // Do a final sync before background
    if (this.store.isOnline) {
      this.store.sync().catch(console.error)
    }
  }

  /**
   * Start sync loop with adaptive frequency
   */
  startSyncLoop() {
    // Calculate sync interval based on network
    const syncInterval = this.isSlowNetwork ? 15000 : 5000 // 15s or 5s

    if (this.syncTimeoutId) {
      clearTimeout(this.syncTimeoutId)
    }

    const sync = async () => {
      if (this.store.isOnline) {
        try {
          await this.store.sync()
          this.lastSuccessfulSync = Date.now()
          this.retryCount = 0
        } catch (error) {
          console.error('Sync error:', error)
          await this.syncWithBackoff()
        }
      }

      // Schedule next sync
      this.syncTimeoutId = setTimeout(sync, syncInterval)
    }

    // Schedule first sync
    this.syncTimeoutId = setTimeout(sync, syncInterval)
  }

  /**
   * Sync with exponential backoff
   */
  async syncWithBackoff() {
    if (this.retryCount >= this.maxRetries) {
      console.error('Max retries reached')
      return
    }

    const delay = this.baseRetryDelay * Math.pow(2, this.retryCount)
    console.log(`Retrying sync after ${delay}ms (attempt ${this.retryCount + 1})`)

    await new Promise((resolve) => setTimeout(resolve, delay))

    try {
      await this.store.sync()
      this.retryCount = 0
    } catch (error) {
      this.retryCount++
      if (this.retryCount < this.maxRetries) {
        await this.syncWithBackoff()
      }
    }
  }

  /**
   * Start network quality monitoring with ping
   */
  startNetworkQualityMonitoring() {
    if (this.pingIntervalId) {
      clearInterval(this.pingIntervalId)
    }

    this.pingIntervalId = setInterval(async () => {
      if (this.store.isOnline) {
        await this.checkNetworkLatency()
      }
    }, 10000) // Every 10 seconds
  }

  /**
   * Check network latency with ping
   */
  async checkNetworkLatency() {
    try {
      const startTime = Date.now()

      // Send lightweight ping request
      const response = await fetch(
        `${process.env.REACT_NATIVE_API_URL}/api/health`,
        {
          method: 'GET',
          timeout: 5000,
        }
      )

      const latency = Date.now() - startTime

      // Update store with latency
      this.store.updateNetworkLatency(latency)

      // Determine if slow based on latency
      this.isSlowNetwork = latency > 500 // >500ms is slow
    } catch (error) {
      console.error('Ping failed:', error)
      this.isSlowNetwork = true
    }
  }

  /**
   * Adjust sync frequency based on network
   */
  adjustSyncFrequency(networkType, isSlowNetwork) {
    console.log(`Network type: ${networkType}, Slow: ${isSlowNetwork}`)

    // Suspend aggressive syncing on slow networks
    if (isSlowNetwork) {
      // Already handled in startSyncLoop
    }

    // Different behavior per platform
    if (Platform.OS === 'ios') {
      // iOS has better background sync support
    } else {
      // Android might need different approach
    }
  }

  /**
   * Send request with retry logic
   */
  async sendWithRetry(url, options = {}, retries = 3) {
    let lastError

    for (let attempt = 0; attempt < retries; attempt++) {
      try {
        const response = await fetch(url, {
          ...options,
          timeout: 10000,
        })

        if (!response.ok) {
          if (response.status === 429) {
            // Rate limited - back off longer
            await new Promise((resolve) =>
              setTimeout(resolve, 5000 * (attempt + 1))
            )
            continue
          }
          throw new Error(`HTTP ${response.status}: ${response.statusText}`)
        }

        return response
      } catch (error) {
        lastError = error
        console.warn(
          `Request attempt ${attempt + 1}/${retries} failed:`,
          error.message
        )

        if (attempt < retries - 1) {
          // Wait before retrying with exponential backoff
          const delay = 1000 * Math.pow(2, attempt)
          await new Promise((resolve) => setTimeout(resolve, delay))
        }
      }
    }

    throw new Error(
      `Request failed after ${retries} retries: ${lastError?.message}`
    )
  }

  /**
   * Check if network is available
   */
  async isNetworkAvailable() {
    try {
      const state = await NetInfo.fetch()
      return state.isConnected
    } catch (error) {
      console.error('Failed to check network:', error)
      return false
    }
  }

  /**
   * Get network info
   */
  getNetworkInfo() {
    return {
      isConnected: this.connectionState?.isConnected || false,
      type: this.connectionState?.type || 'unknown',
      isSlowNetwork: this.isSlowNetwork,
      lastSuccessfulSync: this.lastSuccessfulSync,
    }
  }

  /**
   * Cleanup
   */
  cleanup() {
    if (this.networkSubscription) {
      this.networkSubscription()
    }

    if (this.appStateSubscription) {
      this.appStateSubscription.remove()
    }

    if (this.pingIntervalId) {
      clearInterval(this.pingIntervalId)
    }

    if (this.syncTimeoutId) {
      clearTimeout(this.syncTimeoutId)
    }
  }
}

export default MobileNetworkManager
