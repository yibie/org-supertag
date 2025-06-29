#!/usr/bin/env python3
"""
日志监控服务 - 用于实时跟踪多线程处理进度
"""

import logging
import time
import threading
import queue
from typing import Dict, List, Any, Optional, Callable
from dataclasses import dataclass, field
from collections import deque

logger = logging.getLogger(__name__)

@dataclass
class ProcessingEvent:
    """处理事件"""
    timestamp: float
    event_type: str  # "task_start", "task_complete", "task_error", "progress_update", "system_info"
    worker_id: Optional[int] = None
    task_id: Optional[str] = None
    message: str = ""
    metadata: Dict[str, Any] = field(default_factory=dict)

@dataclass
class WorkerStats:
    """工作进程统计"""
    worker_id: int
    pid: Optional[int] = None
    tasks_completed: int = 0
    tasks_failed: int = 0
    total_processing_time: float = 0.0
    avg_processing_time: float = 0.0
    current_task: Optional[str] = None
    last_activity: float = 0.0
    status: str = "idle"  # "idle", "busy", "error", "offline"

@dataclass
class ProcessingSession:
    """处理会话"""
    session_id: str
    session_type: str  # "embedding", "ner", "mixed"
    total_tasks: int
    completed_tasks: int = 0
    failed_tasks: int = 0
    start_time: float = 0.0
    estimated_completion_time: Optional[float] = None
    throughput: float = 0.0  # tasks per second
    workers: Dict[int, WorkerStats] = field(default_factory=dict)

class LogMonitor:
    """日志监控器"""
    
    def __init__(self, max_events: int = 1000, max_sessions: int = 10):
        self.max_events = max_events
        self.max_sessions = max_sessions
        
        # 事件存储
        self.events = deque(maxlen=max_events)
        self.event_queue = queue.Queue()
        
        # 会话管理
        self.sessions: Dict[str, ProcessingSession] = {}
        self.current_session: Optional[str] = None
        
        # 统计信息
        self.global_stats = {
            "total_tasks_processed": 0,
            "total_processing_time": 0.0,
            "error_count": 0,
            "avg_throughput": 0.0,
            "uptime": time.time()
        }
        
        # 监控线程
        self.monitor_thread = None
        self.stop_event = threading.Event()
        self.is_running = False
        
        # 回调函数
        self.progress_callbacks: List[Callable] = []
        
        logger.info("LogMonitor initialized")
    
    def start(self):
        """启动监控器"""
        if self.is_running:
            return
            
        self.is_running = True
        self.stop_event.clear()
        
        self.monitor_thread = threading.Thread(
            target=self._monitor_loop,
            daemon=True
        )
        self.monitor_thread.start()
        
        logger.info("LogMonitor started")
    
    def stop(self):
        """停止监控器"""
        if not self.is_running:
            return
            
        self.is_running = False
        self.stop_event.set()
        
        if self.monitor_thread:
            self.monitor_thread.join(timeout=5)
            self.monitor_thread = None
        
        logger.info("LogMonitor stopped")
    
    def _monitor_loop(self):
        """监控循环"""
        while not self.stop_event.is_set():
            try:
                # 处理事件队列
                self._process_event_queue()
                
                # 更新统计信息
                self._update_statistics()
                
                # 调用进度回调
                self._call_progress_callbacks()
                
                # 清理过期数据
                self._cleanup_expired_data()
                
                time.sleep(1.0)  # 每秒更新一次
                
            except Exception as e:
                logger.error(f"Monitor loop error: {e}")
                time.sleep(5.0)  # 错误后等待更长时间
    
    def _process_event_queue(self):
        """处理事件队列"""
        processed = 0
        while not self.event_queue.empty() and processed < 100:  # 限制每次处理的事件数
            try:
                event = self.event_queue.get_nowait()
                self._handle_event(event)
                processed += 1
            except queue.Empty:
                break
            except Exception as e:
                logger.error(f"Error processing event: {e}")
    
    def _handle_event(self, event: ProcessingEvent):
        """处理单个事件"""
        self.events.append(event)
        
        # 更新会话统计
        if self.current_session and self.current_session in self.sessions:
            session = self.sessions[self.current_session]
            
            if event.event_type == "task_start":
                if event.worker_id is not None:
                    worker = session.workers.get(event.worker_id)
                    if worker:
                        worker.current_task = event.task_id
                        worker.status = "busy"
                        worker.last_activity = event.timestamp
            
            elif event.event_type == "task_complete":
                session.completed_tasks += 1
                if event.worker_id is not None:
                    worker = session.workers.get(event.worker_id)
                    if worker:
                        worker.tasks_completed += 1
                        worker.current_task = None
                        worker.status = "idle"
                        worker.last_activity = event.timestamp
                        
                        # 更新处理时间
                        processing_time = event.metadata.get("processing_time", 0.0)
                        worker.total_processing_time += processing_time
                        if worker.tasks_completed > 0:
                            worker.avg_processing_time = worker.total_processing_time / worker.tasks_completed
            
            elif event.event_type == "task_error":
                session.failed_tasks += 1
                if event.worker_id is not None:
                    worker = session.workers.get(event.worker_id)
                    if worker:
                        worker.tasks_failed += 1
                        worker.current_task = None
                        worker.status = "error"
                        worker.last_activity = event.timestamp
        
        # 更新全局统计
        if event.event_type == "task_complete":
            self.global_stats["total_tasks_processed"] += 1
            processing_time = event.metadata.get("processing_time", 0.0)
            self.global_stats["total_processing_time"] += processing_time
        elif event.event_type == "task_error":
            self.global_stats["error_count"] += 1
    
    def _update_statistics(self):
        """更新统计信息"""
        if not self.current_session or self.current_session not in self.sessions:
            return
            
        session = self.sessions[self.current_session]
        
        # 计算进度
        if session.total_tasks > 0:
            progress = (session.completed_tasks + session.failed_tasks) / session.total_tasks
            
            # 估算完成时间
            if progress > 0.1 and session.start_time > 0:
                elapsed = time.time() - session.start_time
                estimated_total_time = elapsed / progress
                session.estimated_completion_time = session.start_time + estimated_total_time
            
            # 计算吞吐量
            if session.start_time > 0:
                elapsed = time.time() - session.start_time
                if elapsed > 0:
                    session.throughput = (session.completed_tasks + session.failed_tasks) / elapsed
        
        # 更新全局吞吐量
        uptime = time.time() - self.global_stats["uptime"]
        if uptime > 0:
            self.global_stats["avg_throughput"] = self.global_stats["total_tasks_processed"] / uptime
    
    def _call_progress_callbacks(self):
        """调用进度回调函数"""
        if not self.current_session or not self.progress_callbacks:
            return
            
        session = self.sessions.get(self.current_session)
        if not session:
            return
            
        progress_data = {
            "session_id": self.current_session,
            "session_type": session.session_type,
            "progress": (session.completed_tasks + session.failed_tasks) / max(session.total_tasks, 1),
            "completed": session.completed_tasks,
            "failed": session.failed_tasks,
            "total": session.total_tasks,
            "throughput": session.throughput,
            "estimated_completion": session.estimated_completion_time,
            "active_workers": sum(1 for w in session.workers.values() if w.status == "busy")
        }
        
        for callback in self.progress_callbacks:
            try:
                callback(progress_data)
            except Exception as e:
                logger.error(f"Progress callback error: {e}")
    
    def _cleanup_expired_data(self):
        """清理过期数据"""
        current_time = time.time()
        
        # 清理过期会话
        expired_sessions = []
        for session_id, session in self.sessions.items():
            if session_id != self.current_session:
                # 如果会话超过1小时没有活动，清理它
                last_activity = max(
                    (w.last_activity for w in session.workers.values()),
                    default=session.start_time
                )
                if current_time - last_activity > 3600:  # 1小时
                    expired_sessions.append(session_id)
        
        for session_id in expired_sessions:
            del self.sessions[session_id]
            logger.debug(f"Cleaned up expired session: {session_id}")
    
    # 公共接口方法
    
    def start_session(self, session_id: str, session_type: str, total_tasks: int, worker_count: int):
        """开始新的处理会话"""
        # 清理旧会话
        if len(self.sessions) >= self.max_sessions:
            oldest_session = min(self.sessions.keys())
            del self.sessions[oldest_session]
        
        session = ProcessingSession(
            session_id=session_id,
            session_type=session_type,
            total_tasks=total_tasks,
            start_time=time.time()
        )
        
        # 初始化工作进程
        for i in range(worker_count):
            session.workers[i] = WorkerStats(worker_id=i)
        
        self.sessions[session_id] = session
        self.current_session = session_id
        
        # 记录事件
        self.log_event(
            event_type="session_start",
            message=f"Started {session_type} session with {total_tasks} tasks and {worker_count} workers",
            metadata={"session_id": session_id, "session_type": session_type, "total_tasks": total_tasks}
        )
        
        logger.info(f"Started monitoring session: {session_id} ({session_type}, {total_tasks} tasks)")
    
    def end_session(self, session_id: str):
        """结束处理会话"""
        if session_id in self.sessions:
            session = self.sessions[session_id]
            duration = time.time() - session.start_time
            
            self.log_event(
                event_type="session_end",
                message=f"Session completed: {session.completed_tasks}/{session.total_tasks} successful ({duration:.2f}s)",
                metadata={
                    "session_id": session_id,
                    "duration": duration,
                    "completed": session.completed_tasks,
                    "failed": session.failed_tasks,
                    "total": session.total_tasks
                }
            )
            
            if self.current_session == session_id:
                self.current_session = None
            
            logger.info(f"Ended monitoring session: {session_id}")
    
    def log_event(self, event_type: str, message: str = "", worker_id: Optional[int] = None, 
                  task_id: Optional[str] = None, metadata: Optional[Dict[str, Any]] = None):
        """记录事件"""
        event = ProcessingEvent(
            timestamp=time.time(),
            event_type=event_type,
            worker_id=worker_id,
            task_id=task_id,
            message=message,
            metadata=metadata or {}
        )
        
        try:
            self.event_queue.put_nowait(event)
        except queue.Full:
            logger.warning("Event queue is full, dropping event")
    
    def add_progress_callback(self, callback: Callable):
        """添加进度回调函数"""
        self.progress_callbacks.append(callback)
    
    def remove_progress_callback(self, callback: Callable):
        """移除进度回调函数"""
        if callback in self.progress_callbacks:
            self.progress_callbacks.remove(callback)
    
    def get_current_status(self) -> Dict[str, Any]:
        """获取当前状态"""
        if not self.current_session or self.current_session not in self.sessions:
            return {"status": "idle", "message": "No active session"}
        
        session = self.sessions[self.current_session]
        total_processed = session.completed_tasks + session.failed_tasks
        progress = total_processed / max(session.total_tasks, 1)
        
        return {
            "status": "active",
            "session_id": self.current_session,
            "session_type": session.session_type,
            "progress": progress,
            "completed": session.completed_tasks,
            "failed": session.failed_tasks,
            "total": session.total_tasks,
            "throughput": session.throughput,
            "estimated_completion": session.estimated_completion_time,
            "workers": {
                worker_id: {
                    "status": worker.status,
                    "current_task": worker.current_task,
                    "tasks_completed": worker.tasks_completed,
                    "tasks_failed": worker.tasks_failed,
                    "avg_processing_time": worker.avg_processing_time
                }
                for worker_id, worker in session.workers.items()
            }
        }
    
    def get_recent_events(self, count: int = 50) -> List[Dict[str, Any]]:
        """获取最近的事件"""
        recent_events = list(self.events)[-count:]
        return [
            {
                "timestamp": event.timestamp,
                "event_type": event.event_type,
                "worker_id": event.worker_id,
                "task_id": event.task_id,
                "message": event.message,
                "metadata": event.metadata
            }
            for event in recent_events
        ]
    
    def get_global_stats(self) -> Dict[str, Any]:
        """获取全局统计信息"""
        return {
            **self.global_stats,
            "active_sessions": len(self.sessions),
            "current_session": self.current_session,
            "monitor_uptime": time.time() - self.global_stats["uptime"]
        }
    
    def print_status_report(self):
        """打印状态报告"""
        print("\n" + "="*80)
        print("📊 多线程处理状态报告")
        print("="*80)
        
        if not self.current_session:
            print("⏸️  当前没有活跃的处理会话")
            return
        
        session = self.sessions[self.current_session]
        total_processed = session.completed_tasks + session.failed_tasks
        progress = total_processed / max(session.total_tasks, 1) * 100
        
        print(f"🎯 会话: {self.current_session} ({session.session_type})")
        print(f"📈 进度: {total_processed}/{session.total_tasks} ({progress:.1f}%)")
        print(f"✅ 成功: {session.completed_tasks}")
        print(f"❌ 失败: {session.failed_tasks}")
        print(f"🚄 吞吐量: {session.throughput:.2f} tasks/sec")
        
        if session.estimated_completion_time:
            remaining = session.estimated_completion_time - time.time()
            if remaining > 0:
                print(f"⏰ 预计完成: {remaining/60:.1f} 分钟后")
        
        print("\n👷 工作进程状态:")
        for worker_id, worker in session.workers.items():
            status_icon = {"idle": "⏸️", "busy": "🔧", "error": "❌", "offline": "💤"}.get(worker.status, "❓")
            print(f"  Worker {worker_id}: {status_icon} {worker.status}")
            if worker.current_task:
                print(f"    当前任务: {worker.current_task}")
            print(f"    完成: {worker.tasks_completed}, 失败: {worker.tasks_failed}")
            if worker.avg_processing_time > 0:
                print(f"    平均耗时: {worker.avg_processing_time:.3f}s")
        
        print("="*80)

# 全局监控实例
_global_monitor: Optional[LogMonitor] = None

def get_log_monitor() -> LogMonitor:
    """获取全局日志监控实例"""
    global _global_monitor
    if _global_monitor is None:
        _global_monitor = LogMonitor()
        _global_monitor.start()
    return _global_monitor

def start_monitoring():
    """启动全局监控"""
    monitor = get_log_monitor()
    monitor.start()

def stop_monitoring():
    """停止全局监控"""
    global _global_monitor
    if _global_monitor:
        _global_monitor.stop()
        _global_monitor = None 