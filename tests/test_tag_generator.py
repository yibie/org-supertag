import pytest
from unittest.mock import MagicMock, patch
from simtag.tag_generator import TagGenerator
from simtag.ollama_bridge import OllamaBridge

@pytest.fixture
def mock_ollama():
    """创建模拟的 Ollama Bridge"""
    mock = MagicMock(spec=OllamaBridge)
    return mock

@pytest.fixture
def tag_generator(mock_ollama):
    """创建标签生成器实例"""
    return TagGenerator(mock_ollama)

def test_empty_text(tag_generator):
    """测试空文本输入"""
    assert tag_generator.suggest_tags("") == []
    assert tag_generator.suggest_tags(" ") == []
    assert tag_generator.suggest_tags("\n") == []

def test_normal_text(tag_generator, mock_ollama):
    """测试正常文本输入"""
    # 模拟 Ollama 返回标签
    mock_ollama.run.return_value = "python, programming, coding"
    
    # 测试标签生成
    text = "This is a Python programming tutorial"
    tags = tag_generator.suggest_tags(text)
    
    # 验证结果
    assert isinstance(tags, list)
    assert len(tags) > 0
    assert all(isinstance(tag, str) for tag in tags)
    assert "python" in tags
    assert "programming" in tags

def test_limit_parameter(tag_generator, mock_ollama):
    """测试标签数量限制"""
    # 模拟 Ollama 返回多个标签
    mock_ollama.run.return_value = "tag1, tag2, tag3, tag4, tag5, tag6"
    
    # 测试不同的限制
    assert len(tag_generator.suggest_tags("test", limit=3)) <= 3
    assert len(tag_generator.suggest_tags("test", limit=5)) <= 5

def test_invalid_response(tag_generator, mock_ollama):
    """测试无效的响应处理"""
    # 模拟 Ollama 返回无效响应
    mock_ollama.run.return_value = "Please provide more context"
    assert tag_generator.suggest_tags("test") == []
    
    mock_ollama.run.return_value = None
    assert tag_generator.suggest_tags("test") == []

def test_ollama_error(tag_generator, mock_ollama):
    """测试 Ollama 错误处理"""
    # 模拟 Ollama 抛出异常
    mock_ollama.run.side_effect = Exception("API Error")
    assert tag_generator.suggest_tags("test") == []

def test_tag_formatting(tag_generator, mock_ollama):
    """测试标签格式化"""
    # 模拟返回需要格式化的标签
    mock_ollama.run.return_value = "Machine Learning, PYTHON 3, web-development"
    
    tags = tag_generator.suggest_tags("test")
    
    # 验证格式化结果
    assert all(tag.islower() for tag in tags)
    assert all('_' in tag or tag.isalnum() for tag in tags)
    assert all(2 <= len(tag) <= 30 for tag in tags) 