import { useEffect, useRef, useCallback, useState } from 'react'
import ReconnectingWebSocket from 'reconnecting-websocket'
import { useBoardStore } from '../store/boardStore'

const WS_PORT = 35907

export type ConnectionState = 'connecting' | 'connected' | 'disconnected'

export function useWebSocket() {
  const wsRef = useRef<ReconnectingWebSocket | null>(null)
  const { setBoards, setBoardData, setAvailableNodes, setFollowedNode } = useBoardStore()
  const [connectionState, setConnectionState] = useState<ConnectionState>('connecting')
  const [sendFailed, setSendFailed] = useState(false)

  useEffect(() => {
    const ws = new ReconnectingWebSocket(`ws://localhost:${WS_PORT}`)
    wsRef.current = ws

    ws.onopen = () => {
      setConnectionState('connected')
      // Explicitly request board list on connect (Emacs also pushes it, but this is more robust)
      ws.send(JSON.stringify({ command: 'list-boards', data: null }))
    }

    ws.onclose = () => {
      setConnectionState('disconnected')
    }

    ws.onerror = () => {
      setConnectionState('disconnected')
    }

    ws.onmessage = (event) => {
      try {
        const msg = JSON.parse(event.data)
        const { type, data } = msg

        switch (type) {
          case 'board-list': {
            setBoards(data)
            // #4: If the current board was deleted, clear the canvas
            const { currentBoardId, clearBoard } = useBoardStore.getState()
            if (currentBoardId) {
              const stillExists = data.some((b: { id: string }) => b.id === currentBoardId)
              if (!stillExists) {
                clearBoard()
              }
            }
            break
          }
          case 'board-data':
            setBoardData(data)
            break
          case 'node-list':
            setAvailableNodes(data)
            break
          case 'follow':
            setFollowedNode(data?.id || null)
            break
          case 'store-changed': {
            // Re-request current board data
            const boardId = useBoardStore.getState().currentBoardId
            if (boardId) {
              sendCommand('open-board', { boardId })
            }
            break
          }
        }
      } catch (e) {
        console.error('WebSocket message error:', e)
      }
    }

    return () => {
      ws.close()
      wsRef.current = null
    }
  }, [])

  const sendCommand = useCallback((command: string, data?: any) => {
    if (wsRef.current?.readyState === WebSocket.OPEN) {
      wsRef.current.send(JSON.stringify({ command, data }))
      setSendFailed(false)
    } else {
      console.warn(`[useWebSocket] Cannot send "${command}": connection is not open`)
      setSendFailed(true)
      // Auto-clear the failed indicator after 2s
      setTimeout(() => setSendFailed(false), 2000)
    }
  }, [])

  return { sendCommand, connectionState, sendFailed }
}
