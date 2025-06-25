import { defineStore } from 'pinia'
import { ref, computed, readonly } from 'vue'

export interface User {
  id: string
  username: string
  email?: string
}

export const useAuthStore = defineStore('auth', () => {
  // State
  const user = ref<User | null>(null)
  const isAuthenticated = ref<boolean>(false)
  const isLoading = ref<boolean>(false)
  const error = ref<string | null>(null)

  // Getters
  const currentUser = computed(() => user.value)
  const isLoggedIn = computed(() => isAuthenticated.value && user.value !== null)

  // Actions
  const login = async (username: string, password: string) => {
    isLoading.value = true
    error.value = null

    try {
      // Simulate API call - replace with actual authentication logic
      await new Promise(resolve => setTimeout(resolve, 1000))
      
      // For demo purposes, accept any username/password combination
      if (username && password) {
        user.value = {
          id: '1',
          username,
          email: `${username}@homelab.local`
        }
        isAuthenticated.value = true
        localStorage.setItem('isAuthenticated', 'true')
        localStorage.setItem('user', JSON.stringify(user.value))
        return { success: true }
      } else {
        throw new Error('Username and password are required')
      }
    } catch (err) {
      error.value = err instanceof Error ? err.message : 'Login failed'
      return { success: false, error: error.value }
    } finally {
      isLoading.value = false
    }
  }

  const logout = () => {
    user.value = null
    isAuthenticated.value = false
    error.value = null
    localStorage.removeItem('isAuthenticated')
    localStorage.removeItem('user')
  }

  const initializeAuth = () => {
    const stored = localStorage.getItem('isAuthenticated')
    const storedUser = localStorage.getItem('user')
    
    if (stored === 'true' && storedUser) {
      try {
        user.value = JSON.parse(storedUser)
        isAuthenticated.value = true
      } catch {
        logout()
      }
    }
  }

  const clearError = () => {
    error.value = null
  }

  return {
    // State
    user: readonly(user),
    isAuthenticated: readonly(isAuthenticated),
    isLoading: readonly(isLoading),
    error: readonly(error),
    
    // Getters
    currentUser,
    isLoggedIn,
    
    // Actions
    login,
    logout,
    initializeAuth,
    clearError
  }
})
