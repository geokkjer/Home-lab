<template>
  <DashboardLayout
    :user="currentUser"
    @logout="handleLogout"
    @section-change="handleSectionChange"
  >
    <div class="space-y-6">
      <!-- Dashboard Header -->
      <div class="border-b border-gray-200 pb-5">
        <h1 class="text-3xl font-bold leading-tight text-gray-900">
          Dashboard Overview
        </h1>
        <p class="mt-2 max-w-4xl text-sm text-gray-500">
          Real-time monitoring and status of your home lab infrastructure
        </p>
      </div>

      <!-- Error Alert -->
      <ErrorAlert 
        v-if="error"
        :message="error"
        @dismiss="clearError"
      />

      <!-- Loading State -->
      <LoadingSpinner v-if="isLoading && !systemInfo" />

      <!-- Dashboard Content -->
      <template v-else>
        <!-- System Overview Cards -->
        <div class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6">
          <SystemCard
            v-if="systemInfo"
            :system-info="systemInfo"
            :memory-usage="memoryUsagePercent"
            :disk-usage="diskUsagePercent"
          />
          
          <ServiceStatusCard
            :running-count="runningServices.length"
            :stopped-count="stoppedServices.length"
            :error-count="errorServices.length"
            :total-count="services.length"
          />
          
          <UptimeCard
            v-if="systemInfo"
            :uptime="systemInfo.uptime"
            :hostname="systemInfo.hostname"
          />
          
          <LoadAverageCard
            v-if="systemInfo"
            :load="systemInfo.load"
          />
        </div>

        <!-- Services Table -->
        <div class="bg-white shadow rounded-lg">
          <div class="px-4 py-5 sm:p-6">
            <h3 class="text-lg leading-6 font-medium text-gray-900 mb-4">
              Service Status
            </h3>
            <ServicesTable 
              :services="services"
              @refresh="refreshData"
            />
          </div>
        </div>

        <!-- Quick Actions -->
        <div class="bg-white shadow rounded-lg">
          <div class="px-4 py-5 sm:p-6">
            <h3 class="text-lg leading-6 font-medium text-gray-900 mb-4">
              Quick Actions
            </h3>
            <QuickActions @action="handleQuickAction" />
          </div>
        </div>
      </template>
    </div>
  </DashboardLayout>
</template>

<script setup lang="ts">
import { onMounted } from 'vue'
import { useRouter } from 'vue-router'
import { storeToRefs } from 'pinia'
import { useAuthStore } from '@/stores/auth'
import { useDashboardStore } from '@/stores/dashboard'

// Layout and UI Components
import DashboardLayout from '@/layouts/DashboardLayout.vue'
import ErrorAlert from '@/components/ErrorAlert.vue'
import LoadingSpinner from '@/components/LoadingSpinner.vue'

// Dashboard Components
import SystemCard from '@/components/SystemCard.vue'
import ServiceStatusCard from '@/components/ServiceStatusCard.vue'
import UptimeCard from '@/components/UptimeCard.vue'
import LoadAverageCard from '@/components/LoadAverageCard.vue'
import ServicesTable from '@/components/ServicesTable.vue'
import QuickActions from '@/components/QuickActions.vue'

const router = useRouter()
const authStore = useAuthStore()
const dashboardStore = useDashboardStore()

const { currentUser } = storeToRefs(authStore)
const { 
  systemInfo, 
  services, 
  isLoading, 
  error,
  runningServices,
  stoppedServices,
  errorServices,
  memoryUsagePercent,
  diskUsagePercent
} = storeToRefs(dashboardStore)

const handleLogout = () => {
  authStore.logout()
  router.push('/login')
}

const handleSectionChange = (section: string) => {
  console.log('Section changed to:', section)
  // Handle section changes - could trigger different data fetching
}

const refreshData = () => {
  dashboardStore.refreshData()
}

const clearError = () => {
  dashboardStore.clearError()
}

const handleQuickAction = (action: string) => {
  console.log('Quick action:', action)
  // Handle quick actions like restarting services, etc.
}

// Initialize dashboard data on mount
onMounted(() => {
  dashboardStore.refreshData()
})
</script>
