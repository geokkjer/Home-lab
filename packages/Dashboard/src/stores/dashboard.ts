import { defineStore } from 'pinia'
import { ref, computed, readonly } from 'vue'

export interface SystemInfo {
  hostname: string
  uptime: string
  load: readonly number[]
  memory: {
    total: number
    used: number
    free: number
  }
  disk: {
    total: number
    used: number
    free: number
  }
}

export interface ServiceStatus {
  name: string
  status: 'running' | 'stopped' | 'error' | 'unknown'
  port?: number
  url?: string
  description?: string
}

export const useDashboardStore = defineStore('dashboard', () => {
  // State
  const systemInfo = ref<SystemInfo | null>(null)
  const services = ref<ServiceStatus[]>([])
  const isLoading = ref<boolean>(false)
  const error = ref<string | null>(null)
  const lastUpdated = ref<Date | null>(null)

  // Getters
  const runningServices = computed(() => 
    services.value.filter(service => service.status === 'running')
  )
  
  const stoppedServices = computed(() => 
    services.value.filter(service => service.status === 'stopped')
  )
  
  const errorServices = computed(() => 
    services.value.filter(service => service.status === 'error')
  )

  const memoryUsagePercent = computed(() => {
    if (!systemInfo.value) return 0
    const { total, used } = systemInfo.value.memory
    return Math.round((used / total) * 100)
  })

  const diskUsagePercent = computed(() => {
    if (!systemInfo.value) return 0
    const { total, used } = systemInfo.value.disk
    return Math.round((used / total) * 100)
  })

  // Actions
  const fetchSystemInfo = async () => {
    isLoading.value = true
    error.value = null

    try {
      // Simulate API call - replace with actual system info fetching
      await new Promise(resolve => setTimeout(resolve, 500))
      
      // Mock data for demonstration
      systemInfo.value = {
        hostname: 'congenital-optimist',
        uptime: '5 days, 14 hours',
        load: [0.15, 0.25, 0.30],
        memory: {
          total: 16384,
          used: 8192,
          free: 8192
        },
        disk: {
          total: 1000000,
          used: 450000,
          free: 550000
        }
      }
      
      lastUpdated.value = new Date()
    } catch (err) {
      error.value = err instanceof Error ? err.message : 'Failed to fetch system info'
    } finally {
      isLoading.value = false
    }
  }

  const fetchServices = async () => {
    try {
      // Simulate API call - replace with actual service status fetching
      await new Promise(resolve => setTimeout(resolve, 300))
      
      // Mock data for demonstration
      services.value = [
        {
          name: 'Netdata',
          status: 'running',
          port: 19999,
          url: 'http://localhost:19999',
          description: 'Real-time performance monitoring'
        },
        {
          name: 'Ollama',
          status: 'running',
          port: 11434,
          url: 'http://localhost:11434',
          description: 'Local LLM inference server'
        },
        {
          name: 'SSH',
          status: 'running',
          port: 22,
          description: 'Secure Shell access'
        },
        {
          name: 'NFS',
          status: 'running',
          port: 2049,
          description: 'Network File System'
        },
        {
          name: 'Reverse Proxy',
          status: 'error',
          port: 80,
          description: 'HTTP reverse proxy'
        }
      ]
    } catch (err) {
      error.value = err instanceof Error ? err.message : 'Failed to fetch services'
    }
  }

  const refreshData = async () => {
    await Promise.all([
      fetchSystemInfo(),
      fetchServices()
    ])
  }

  const clearError = () => {
    error.value = null
  }

  return {
    // State
    systemInfo: readonly(systemInfo),
    services: readonly(services),
    isLoading: readonly(isLoading),
    error: readonly(error),
    lastUpdated: readonly(lastUpdated),
    
    // Getters
    runningServices,
    stoppedServices,
    errorServices,
    memoryUsagePercent,
    diskUsagePercent,
    
    // Actions
    fetchSystemInfo,
    fetchServices,
    refreshData,
    clearError
  }
})
