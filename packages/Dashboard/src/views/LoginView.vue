<template>
  <div class="min-h-screen bg-gray-50 flex items-center justify-center py-12 px-4 sm:px-6 lg:px-8">
    <div class="max-w-md w-full space-y-8">
      <div>
        <div class="mx-auto h-12 w-12 flex items-center justify-center bg-blue-600 rounded-lg">
          <svg class="h-8 w-8 text-white" fill="none" viewBox="0 0 24 24" stroke="currentColor">
            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 3v2m6-2v2M9 19v2m6-2v2M5 9H3m2 6H3m18-6h-2m2 6h-2M7 19h10a2 2 0 002-2V7a2 2 0 00-2-2H7a2 2 0 00-2 2v10a2 2 0 002 2zM9 9h6v6H9V9z" />
          </svg>
        </div>
        <h2 class="mt-6 text-center text-3xl font-extrabold text-gray-900">
          Home Lab Dashboard
        </h2>
        <p class="mt-2 text-center text-sm text-gray-600">
          Sign in to access your infrastructure monitoring
        </p>
      </div>

      <LoginForm
        :is-loading="isLoading"
        :error="error"
        @submit="handleLogin"
        @clear-error="clearError"
      />
    </div>
  </div>
</template>

<script setup lang="ts">
import { useRouter } from 'vue-router'
import { useAuthStore } from '@/stores/auth'
import { storeToRefs } from 'pinia'
import LoginForm from '@/components/LoginForm.vue'

const router = useRouter()
const authStore = useAuthStore()
const { isLoading, error } = storeToRefs(authStore)

const handleLogin = async (credentials: { username: string; password: string }) => {
  const result = await authStore.login(credentials.username, credentials.password)
  
  if (result.success) {
    router.push('/dashboard')
  }
}

const clearError = () => {
  authStore.clearError()
}
</script>
