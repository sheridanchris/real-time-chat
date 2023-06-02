import { defineConfig } from 'vite'

export default defineConfig({
  root: "src/RealTimeChat.Client",
  server: {
    proxy: {
      '/ws': {
        target: 'http://0.0.0.0:5000',
        ws: true
      }
    }
  }
})